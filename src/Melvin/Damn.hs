module Melvin.Damn (
  packetStream,
  responder
) where

import           Control.Arrow
import           Control.Concurrent
import           Control.Exception           (fromException, throwIO)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Proxy
import           Control.Proxy.Safe
import           Control.Proxy.Trans.State
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Melvin.Chatrooms
import           Melvin.Client.Packet hiding (Packet(..), parse, render)
import qualified Melvin.Damn.Actions as Damn
import           Melvin.Exception
import           Melvin.Logger
import           Melvin.Prelude
import           Melvin.Types
import           System.IO hiding            (isEOF, print, putStrLn)
import           System.IO.Error
import           Text.Damn.Packet hiding     (render)

hGetTillNull :: Handle -> IO Text
hGetTillNull h = do
    ready <- hWaitForInput h 180000
    if ready
        then do
            ch <- hGetChar h
            if ch == '\0'
                then return mempty
                else fmap (T.cons ch) $ hGetTillNull h
        else throwIO $ mkIOError eofErrorType "read timeout" (Just h) Nothing

handler :: SomeException
        -> ClientP a' a b' b SafeIO ()
handler ex | Just (ClientSocketErr e) <- fromException ex = do
    logWarning $ formatS "Server thread hit an exception, but client disconnected ({}), so nothing to do." [show e]
    throw ex
handler ex = do
    uname <- liftP $ gets (view username)
    writeClient $ rplNotify uname $ formatS "Error when communicating with dAmn: {}" [show ex]
    if isRetryable ex
        then writeClient $ rplNotify uname "Trying to reconnect..."
        else do
            writeClient $ rplNotify uname "Unrecoverable error. Disconnecting..."
            killClient
    throw ex

packetStream :: MVar Handle -> () -> Producer ClientP Packet SafeIO ()
packetStream mv () = bracket id
    (do hndl <- readMVar mv
        auth hndl
        return hndl)
    hClose
    (\h -> handle handler $ fix $ \f -> do
        isEOF <- tryIO $ hIsEOF h
        isClosed <- tryIO $ hIsClosed h
        when (isEOF || isClosed) $ throw (ServerDisconnect "socket closed")
        line <- tryIO $ hGetTillNull h
        logWarning $ show line
        case parse $ cleanup line of
            Left err -> throw $ ServerNoParse err line
            Right pk -> do
                respond pk
                f)
    where cleanup m = fromMaybe m $ T.stripSuffix "\n" m

responder :: () -> Consumer ClientP Packet SafeIO ()
responder () = handle handler $ fix $ \f -> do
    p <- request ()
    continue <- case M.lookup (pktCommand p) responses of
        Nothing -> do
            logInfo $ formatS "Unhandled packet from damn: {}" [show p]
            return False
        Just callback -> do
            st <- liftP get
            callback p st
    when continue f

auth :: Handle -> IO ()
auth h = hprint h "dAmnClient 0.3\nagent=melvin 0.1\n\0" ()


-- | Big ol' list of callbacks!
type Callback = Packet -> ClientSettings -> Consumer ClientP Packet SafeIO Bool

type RecvCallback = Packet -> Callback

responses :: M.Map Text Callback
responses = M.fromList [ ("dAmnServer", res_dAmnServer)
                       , ("login", res_login)
                       , ("join", res_join)
                       , ("property", res_property)
                       , ("recv", res_recv)
                       , ("disconnect", res_disconnect)
                       ]

recv_responses :: M.Map Text RecvCallback
recv_responses = M.fromList [ ("msg", res_recv_msg) ]

res_dAmnServer :: Callback
res_dAmnServer _ st = do
    let (num, (user, tok)) = (clientNumber &&& view username &&& view token) st
    logInfo $ formatS "Client #{} handshook successfully." [num]
    writeServer $ Damn.login user tok
    return True

res_login :: Callback
res_login Packet { pktArgs = args } st = do
    let user = st ^. username
    case args ^. ix "e" of
        "ok" -> do
            modifyState (loggedIn .~ True)
            writeClient $ rplNotify user "Authenticated successfully."
            joinlist <- getsState (view joinList)
            forM_ (S.elems joinlist) Damn.join
            return True
        x -> do
            writeClient $ rplNotify user "Authentication failed!"
            throw $ AuthenticationFailed x

res_join :: Callback
res_join Packet { pktParameter = p
                , pktArgs = args } st = do
    let user = st ^. username
    channel <- toChannel $ fromJust p
    case args ^. ix "e" of
        "ok" -> writeClient $ cmdJoin user channel
        "not privileged" -> writeClient $ errBannedFromChan user channel
        _ -> return ()
    return True

res_property :: Callback
res_property Packet { pktParameter = p
                    , pktArgs = args
                    , pktBody = body } st = do
    let user = st ^. username
    channel <- toChannel $ fromJust p
    case args ^. ix "p" of
        "topic" -> case body of
            Nothing -> writeClient $ rplNoTopic user channel "No topic is set"
            Just b -> do
                writeClient $ rplTopic user channel b
                writeClient $ rplTopicWhoTime user channel (args ^. ix "by") (args ^. ix "ts")

        "title" -> logInfo $ formatS "Received title for {}: {}" [channel, body ^. _Just]

        "privclasses" -> do
            logInfo $ formatS "Received privclasses for {}" [channel]
            setPrivclasses channel $ body ^. _Just

        "members" -> do
            setMembers channel $ body ^. _Just
            m <- getsState (\s -> s ^. users ^?! ix (toChatroom channel ^?! _Just))
            writeClient $ rplNameReply channel user (map renderUser $ M.elems m)

        x -> logError $ formatS "unhandled property {}" [x]

    return True
    where
        setPrivclasses c b = do
            let pcs = toPrivclasses $
                  ((T.decimal *** T.tail) . T.breakOn ":") <$> T.splitOn "\n" b
                chat = toChatroom c ^?! _Just
            modifyState (privclasses . at chat ?~ pcs)

        -- decimal x returns a thing like Right (number, text)
        toPrivclasses ((Right (n,_),t):ns) = M.insert t (mkPrivclass n t) (toPrivclasses ns)
        toPrivclasses ((Left _,_):ns) = toPrivclasses ns
        toPrivclasses [] = M.empty

        setMembers c b = do
            let chat = toChatroom c ^?! _Just
            pcs <- getsState (\s -> s ^. privclasses ^?! ix chat)
            let users_ = foldr (toUser pcs) M.empty $ T.splitOn "\n\n" b
            modifyState (users . at chat ?~ users_)

        toUser pcs text = M.insert uname $
                mkUser pcs uname (g "pc")
                                 (read . T.unpack $ g "usericon")
                                 (T.head $ g "symbol")
                                 (g "realname")
                                 (g "gpc")
            where (header:as) = T.splitOn "\n" text
                  uname = last $ T.splitOn " " header
                  attrs = map (second T.tail . T.breakOn "=") as
                  g k = fromJust $ lookup k attrs

res_disconnect :: Callback
res_disconnect Packet { pktArgs = args } st = do
    let user = st ^. username
    case args ^. ix "e" of
        "ok" -> return False
        n -> do
            writeClient $ rplNotify user $ "Disconnected: " ++ n
            throw $ ServerDisconnect n

res_recv :: Callback
res_recv pk st = case pk ^. pktSubpacketL of
    Nothing -> logError (formatS "Received an empty recv packet: {}" [show pk]) >> return True
    Just spk -> case M.lookup (pktCommand spk) recv_responses of
                    Nothing -> logError (formatS "Unhandled recv packet: {}" [show spk]) >> return True
                    Just c -> c pk spk st

res_recv_msg :: RecvCallback
res_recv_msg Packet { pktParameter = p }
             Packet { pktArgs = args
                    , pktBody = b
                    } _st = do
    channel <- toChannel $ fromJust p
    writeClient $ cmdPrivmsg (args ^. ix "from") channel (T.cons ':' $ b ^. _Just)
    return True

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
