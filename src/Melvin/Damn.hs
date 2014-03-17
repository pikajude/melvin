{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Melvin.Damn (loop) where

import           Control.Applicative
import           Control.Arrow hiding             (loop)
import           Control.Concurrent.Lifted hiding (yield)
import           Control.Exception                (throw)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fix
import qualified Data.ByteString as B
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Melvin.Chatrooms
import           Melvin.Client.Packet hiding      (Packet(..), parse, render)
import qualified Melvin.Damn.Actions as Damn
import           Melvin.Damn.Tablumps
import           Melvin.Exception
import           Melvin.Prelude
import           Melvin.Types
import           Network
import           System.IO hiding                 (isEOF, print, putStrLn, utf8)
import           Text.Damn.Packet hiding          (render)

handler :: ClientT m => SomeException -> m ()
handler ex | Just (ClientSocketErr e) <- fromException ex = do
    $logWarn $ [st|Server thread hit an exception, but client disconnected (%?), so nothing to do.|] e
    throwM ex
handler ex | Just ThreadKilled <- fromException ex = return ()
handler ex = do
    writeClient . rplNotify $ [st|Error when communicating with dAmn: %?|] ex
    if isRetryable ex
        then writeClient $ rplNotify "Trying to reconnect..."
        else do
            writeClient $ rplNotify "Unrecoverable error. Disconnecting..."
            killClient
    throwM ex

establishConnection :: ClientT m => m ()
establishConnection = do
    smv <- use serverMVar
    void $ tryTakeMVar smv
    h <- liftIO $ connectTo "chat.deviantart.com" (PortNumber 3900)
    putMVar smv h

loop :: ClientT m => m ()
loop = do
    handle handler establishConnection
    mv <- use serverMVar
    bracket
        (do hndl <- readMVar mv
            liftIO $ auth hndl
            return hndl)
        (liftIO . hClose)
        (\h -> handle handler $ runT_ $
            reading h 8192
                ~> endingBy "\0"
                ~> pass ($logDebug . show)
                ~> auto (liftM2 (,) id (parse . cleanup))
                ~> handleServer)
    where cleanup m = if "\n" `B.isSuffixOf` m
                          then cleanup (B.init m)
                          else m

handleServer :: (Category k, ClientT m)
             => MachineT m (k (ByteString, Either String Packet)) ()
handleServer = construct $ fix $ \f -> do
    res <- await
    case res of
        (str, Left e) -> throw $ ServerNoParse e str
        (_, Right p) -> do
            continue <- case M.lookup (pktCommand p) responses of
                Nothing -> do
                    $logWarn $ [st|Unhandled packet from damn: %?|] p
                    return False
                Just callback -> do
                    sta <- get
                    lift $ callback p sta
            when continue f

auth :: Handle -> IO ()
auth h = System.IO.hPutStr h "dAmnClient 0.3\nagent=melvin 0.1\n\0"


-- | Big ol' list of callbacks!
type Callback m = Packet -> ClientSettings -> m Bool

type RecvCallback m = Packet -> Callback m

responses :: ClientT m => M.Map Text (Callback m)
responses = M.fromList [ ("dAmnServer", res_dAmnServer)
                       , ("ping", res_ping)
                       , ("login", res_login)
                       , ("join", res_join)
                       , ("part", res_part)
                       , ("property", res_property)
                       , ("recv", res_recv)
                       , ("kicked", res_kicked)
                       , ("send", res_send)
                       , ("disconnect", res_disconnect)
                       ]

recv_responses :: ClientT m => M.Map Text (RecvCallback m)
recv_responses = M.fromList [ ("msg", res_recv_msg)
                            , ("action", res_recv_action)
                            , ("join", res_recv_join)
                            , ("part", res_recv_part)
                            , ("privchg", res_recv_privchg)
                            , ("admin", res_recv_admin)
                            ]

res_dAmnServer :: ClientT m => Callback m
res_dAmnServer _ sta = do
    let (num, (user, tok)) = (clientNumber &&& view username &&& view token) sta
    $logDebug $ [st|Client #%d handshook successfully.|] num
    writeServer $ Damn.login user tok
    return True

res_ping :: ClientT m => Callback m
res_ping _ _ = writeServer Damn.pong >> return True

res_login :: ClientT m => Callback m
res_login Packet { pktArgs = args } _ =
    case args ^. ix "e" of
        "ok" -> do
            modifyState (loggedIn .~ True)
            writeClient $ rplNotify "Authenticated successfully."
            joinlist <- getsState (view joinList)
            forM_ (S.elems joinlist) $ writeServer <=< Damn.join
            return True
        x -> do
            writeClient $ rplNotify "Authentication failed!"
            throwM $ AuthenticationFailed x

res_join :: ClientT m => Callback m
res_join Packet { pktParameter = p
                , pktArgs = args } sta = do
    let user = sta ^. username
    channel <- toChannel $ $fromJst p
    case args ^. ix "e" of
        "ok" -> do
            modifyState (joining %~ S.insert channel)
            writeClient $ cmdJoin user channel
        "not privileged" -> writeClient $ errBannedFromChan user channel
        _ -> return ()
    return True

res_part :: ClientT m => Callback m
res_part Packet { pktParameter = p
                , pktArgs = args } sta = do
    let user = sta ^. username
    channel <- toChannel $ $fromJst p
    case args ^. ix "e" of
        "ok" -> writeClient $ cmdPart user channel "leaving"
        _ -> return ()
    return True

res_property :: ClientT m => Callback m
res_property Packet { pktParameter = p
                    , pktArgs = args
                    , pktBody = body } sta = do
    let user = sta ^. username
    channel <- toChannel $ $fromJst p
    case args ^. ix "p" of
        "topic" -> case body of
            Nothing -> writeClient $ rplNoTopic user channel "No topic is set"
            Just b -> do
                writeClient $ rplTopic user channel (T.cons ':' . T.intercalate " | " . linesOf $ delump $ utf8 b)
                writeClient $ rplTopicWhoTime user channel (args ^. ix "by") (args ^. ix "ts")

        "title" -> $logDebug $ [st|Received title for %s: %s|] channel (body ^. _Just)

        "privclasses" -> do
            $logDebug $ [st|Received privclasses for %s|] channel
            joining' <- getsState (view joining)
            unless (channel `S.member` joining') $ do
                $logDebug $ [st|Got privclasses, but finished joining!|]
                $logDebug $ [st|%?|] (toPrivclasses (body ^. _Just . to utf8))
            modifyState $ privclasses . at (toChatroom channel ^?! _Just) ?~
                toPrivclasses (body ^. _Just . to utf8)

        "members" -> do
            setMembers channel $ body ^. _Just . to utf8
            joining' <- getsState (view joining)
            when (channel `S.member` joining') $ do
                let room = toChatroom channel ^?! _Just
                m <- getsState (\s -> s ^. users ^?! ix room)
                writeClient $ rplNameReply channel user (map renderUser $ M.elems m)
                mypc <- getsState (\s -> s ^?! users . ix room . ix (sta ^. username) . userPrivclass)
                writeClient $ cmdModeUpdate channel user Nothing (mypc >>= asMode)
                modifyState (joining %~ S.delete channel)

        x -> $logError $ [st|Unhandled property %s|] x

    return True
    where
        toPrivclasses bod = go $ ((T.decimal *** T.tail) . T.breakOn ":")
            <$> T.splitOn "\n" bod

        -- decimal x returns a thing like Right (number, text)
        go ((Right (n,_),t):ns) = M.insert t (mkPrivclass n t) (go ns)
        go ((Left _,_):ns) = go ns
        go [] = M.empty

        setMembers c b = do
            let chat = toChatroom c ^?! _Just
            pcs <- if T.head c == '&'
                       then return mempty
                       else getsState (\s -> s ^. privclasses ^?! ix chat)
            let users_ = foldr (toUser pcs) M.empty $ T.splitOn "\n\n" b
            modifyState (users . at chat ?~ users_)

        toUser pcs text = M.insertWith (\b _ -> b & userJoinCount +~ 1) uname $
                mkUser pcs uname (g "pc")
                                 (read . T.unpack $ g "usericon")
                                 (T.head $ g "symbol")
                                 (g "realname")
                                 (g "gpc")
            where (header:as) = T.splitOn "\n" text
                  uname = last $ T.splitOn " " header
                  attrs = map (second T.tail . T.breakOn "=") as
                  g k = lookup k attrs ^. _Just

res_send :: ClientT m => Callback m
res_send Packet { pktParameter = p
                , pktArgs = args
                } _ = do
    channel <- toChannel $ p ^. _Just
    writeClient $ cmdSendError channel (args ^. ix "e")
    return True

res_disconnect :: ClientT m => Callback m
res_disconnect Packet { pktArgs = args } _ =
    case args ^. ix "e" of
        "ok" -> return False
        n -> do
            writeClient . rplNotify $ "Disconnected: " ++ n
            throwM $ ServerDisconnect n

res_recv :: ClientT m => Callback m
res_recv pk sta = case pk ^. pktSubpacketL of
    Nothing -> True <$ $logError ([st|Received an empty recv packet: %?|] pk)
    Just spk -> case M.lookup (pktCommand spk) recv_responses of
                    Nothing -> True <$ $logError ([st|Unhandled recv packet: %?|] spk)
                    Just c -> c pk spk sta

res_recv_msg :: ClientT m => RecvCallback m
res_recv_msg Packet { pktParameter = p }
             Packet { pktArgs = args
                    , pktBody = b
                    } sta = do
    channel <- toChannel $ $fromJst p
    unless (sta ^. username == args ^. ix "from") $
        forM_ (linesOf . delump $ b ^. _Just . to utf8) $ \line ->
            writeClient $ cmdPrivmsg (args ^. ix "from") channel line
    return True

res_recv_action :: ClientT m => RecvCallback m
res_recv_action Packet { pktParameter = p }
                Packet { pktArgs = args
                       , pktBody = b
                       } sta = do
    channel <- toChannel $ $fromJst p
    unless (sta ^. username == args ^. ix "from") $
        forM_ (linesOf . delump $ b ^. _Just . to utf8) $ \line ->
            writeClient $ cmdPrivaction (args ^. ix "from") channel line
    return True

res_recv_join :: ClientT m => RecvCallback m
res_recv_join Packet { pktParameter = p }
              Packet { pktParameter = u
                     , pktBody = b
                     } _st = do
    channel <- toChannel $ $fromJst p
    let room = toChatroom channel ^?! _Just
    us <- getsState (\s -> s ^? users . ix room . ix (u ^. _Just))
    case us of
        Nothing -> do
            user <- buildUser (u ^. _Just) (b ^. _Just . to utf8) room
            modifyState (users . ix room %~ M.insert (u ^. _Just) user)
            writeClient $ cmdJoin (u ^. _Just) channel
            case asMode =<< view userPrivclass user of
                Just m -> writeClient $ cmdModeChange channel (u ^. _Just) m
                Nothing -> return ()
        Just r -> do
            modifyState (users . ix room . ix (u ^. _Just) . userJoinCount +~ 1)
            writeClient $ cmdDupJoin (u ^. _Just) channel (r ^. userJoinCount + 1)
    return True
    where
        buildUser name as room = do
            pcs <- getsState (\s -> s ^. privclasses . ix room)
            return $ mkUser pcs name (g "pc")
                                     (read . T.unpack $ g "usericon")
                                     (T.head $ g "symbol")
                                     (g "realname")
                                     (g "gpc")
            where args' = map (second T.tail . T.breakOn "=") (T.splitOn "\n" as)
                  g k = lookup k args' ^. _Just

res_recv_part :: ClientT m => RecvCallback m
res_recv_part Packet { pktParameter = p }
              Packet { pktParameter = u
                     , pktArgs = args
                     } _st = do
    channel <- toChannel $ p ^. _Just
    let room = toChatroom channel ^?! _Just
    us <- getsState (\s -> s ^?! users . ix room . ix (u ^. _Just))
    case us ^. userJoinCount of
        1 -> do
            modifyState (users . ix room %~ M.delete (u ^. _Just))
            writeClient $ cmdPart (u ^. _Just) channel (fromMaybe "no reason" $ args ^? ix "r")
        n -> do
            modifyState (users . ix room . ix (u ^. _Just) . userJoinCount -~ 1)
            writeClient $ cmdDupPart (u ^. _Just) channel (n - 1)
    return True

res_recv_privchg :: ClientT m => RecvCallback m
res_recv_privchg Packet { pktParameter = p }
                 Packet { pktParameter = u
                        , pktArgs = args
                        } _st = do
    channel <- toChannel $ p ^. _Just
    let room = toChatroom channel ^?! _Just
    user <- getsState (\s -> s ^? users . ix room . ix (u ^. _Just))
    case user >>= fmap pcTitle . view userPrivclass of
        -- they're definitely here; possibly-empty Folds are ok
        Just pc' -> do
            pc <- getsState (\s -> s ^?! privclasses . ix room . ix pc')
            newpc <- getsState (\s -> s ^?! privclasses . ix room . ix (args ^. ix "pc"))
            writeClient $ cmdModeUpdate channel (u ^. _Just) (asMode pc) (asMode newpc)
            modifyState (users . ix room . ix (u ^. _Just) . userPrivclass ?~ newpc)

        -- either the user isn't here or they have no PC
        Nothing -> return ()
    writeClient $ cmdPcMove channel (u ^. _Just) (args ^. ix "pc") (args ^. ix "by")
    return True

res_recv_admin :: ClientT m => RecvCallback m
res_recv_admin parent pkt@Packet { pktParameter = cmd } = case cmd of
    Just "create" -> res_recv_admin_update True parent pkt
    Just "update" -> res_recv_admin_update False parent pkt
    Just "remove" -> res_recv_admin_remove parent pkt
    _ -> const $ True <$ $logError ([st|Unhandled admin packet: %?|] pkt)

res_recv_admin_update :: ClientT m => Bool -> RecvCallback m
res_recv_admin_update b
                      Packet { pktParameter = p }
                      Packet { pktArgs = args }
                      _st = do
    channel <- toChannel $ p ^. _Just
    writeClient $ (if b then cmdPcCreate else cmdPcUpdate)
        channel
        (args ^. ix "name")
        (args ^. ix "privs")
        (args ^. ix "by")
    return True

res_recv_admin_remove :: ClientT m => RecvCallback m
res_recv_admin_remove Packet { pktParameter = p }
                      Packet { pktArgs = args }
                      _st = do
    channel <- toChannel $ p ^. _Just
    writeClient $ cmdPcRemove
        channel
        (args ^. ix "name")
        (args ^. ix "by")
    return True

res_kicked :: ClientT m => Callback m
res_kicked Packet { pktParameter = p
                  , pktArgs = args
                  , pktBody = b
                  } sta = do
    channel <- toChannel $ p ^. _Just
    writeClient $ cmdKick (args ^. ix "by") channel (sta ^. username) (utf8 <$> b)
    return True

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
