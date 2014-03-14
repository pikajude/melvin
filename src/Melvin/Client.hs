{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Melvin.Client (loop) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fix
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import           Melvin.Chatrooms
import           Melvin.Client.Packet hiding (render)
import qualified Melvin.Damn.Actions as Damn
import           Melvin.Prelude
import           Melvin.Types

handler :: SomeException -> ClientT ()
handler ex = do
    $logError $ show (ex :: SomeException)
    killServer
    throwM ex

loop :: Handle -> ClientT ()
loop hndl = bracket
    (return hndl)
    (liftIO . hClose)
    (\h -> handle handler $ runT_ $
        reading h 512
            ~> splittingBy "\r\n"
            ~> auto parse
            ~> handleClient)

handleClient :: Category k => MachineT ClientT (k Packet) ()
handleClient = construct $ fix $ \f -> do
    p <- await
    continue <- case M.lookup (pktCommand p) responses of
        Nothing -> do
            $logInfo $ [st|Unhandled packet from client: %?|] p
            return False
        Just callback -> do
            sta <- get
            lift $ callback p sta
    when continue f

-- | Big ol' list of callbacks!
type Callback = Packet -> ClientSettings -> ClientT Bool

responses :: M.Map Text Callback
responses = M.fromList [ ("QUIT", res_quit)
                       , ("PING", res_ping)
                       , ("PONG", \_ _ -> return True)
                       , ("USER", \_ _ -> return True)
                       , ("MODE", res_mode)
                       , ("JOIN", res_join)
                       , ("PART", res_part)
                       , ("PRIVMSG", res_privmsg)
                       ]

res_quit :: Callback
res_quit _ sta = do
    $logInfo $ [st|Client #%d quit cleanly.|] (clientNumber sta)
    writeServer Damn.disconnect
    return False

res_ping :: Callback
res_ping Packet { pktArguments = args } _ = do
    writeClient $ cmdPong args
    return True

res_mode :: Callback
res_mode p _ = do
    $logInfo $ [st|Received mode command, should handle: %?|] p
    return True

res_join :: Callback
res_join Packet { pktArguments = a } sta = do
    case a of
        [] -> writeClient $ errNeedMoreParams (sta ^. username)
        (rooms:_) -> forM_ (T.splitOn "," rooms) $ \r ->
            case toChatroom r of
                Nothing -> writeClient $ errNoSuchChannel (sta ^. username) r
                Just c -> do
                    l <- getsState $ view loggedIn
                    if l
                       then writeServer =<< Damn.join c
                       else modifyState (joinList %~ S.insert c)
    return True

res_part :: Callback
res_part Packet { pktArguments = a } sta = do
    case a of
        [] -> writeClient $ errNeedMoreParams (sta ^. username)
        (room:_) -> case toChatroom room of
            Nothing -> writeClient $ errNoSuchChannel (sta ^. username) room
            Just c -> writeServer =<< Damn.part c
    return True

res_privmsg :: Callback
res_privmsg Packet { pktArguments = (room:msg) } sta = do
    case toChatroom room of
        Nothing -> writeClient $ errNoSuchChannel (sta ^. username) room
        Just r -> case msg of
                      [] -> writeClient $ errNeedMoreParams (sta ^. username)
                      (m:_) -> case T.stripPrefix "\1ACTION " m of
                                   Just ac -> writeServer =<< Damn.action r (T.init ac)
                                   Nothing -> writeServer =<< Damn.msg r m
    return True
res_privmsg _ sta = writeClient (errNeedMoreParams (sta ^. username)) >> return True

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
