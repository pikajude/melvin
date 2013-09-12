module Melvin.Client (
  packetStream,
  responder
) where

import           Control.Monad
import           Control.Monad.Fix
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import           Melvin.Chatrooms
import           Melvin.Client.Packet hiding (render)
import qualified Melvin.Damn.Actions as Damn
import           Melvin.Logger
import           Melvin.Prelude
import           Melvin.Types
import           Pipes.Safe

handler :: SomeException -> ClientT ()
handler ex = do
    logError $ show ex
    killServer
    throwM ex

packetStream :: Handle -> Producer Packet ClientT ()
packetStream hndl = bracket
    (return hndl)
    (liftIO . hClose)
    (\h -> handle (lift . handler) $ fix $ \f -> do
        isEOF <- liftIO $ hIsEOF h
        isClosed <- liftIO $ hIsClosed h
        unless (isEOF || isClosed) $ do
            line <- liftIO $ hGetLine h
            lift $ logInfo line
            yield $ parse line
            f)

responder :: Consumer Packet ClientT ()
responder = handle (lift . handler) $ fix $ \f -> do
    p <- await
    continue <- case M.lookup (pktCommand p) responses of
        Nothing -> do
            lift . logInfo $ formatS "Unhandled packet from client: {}" [show p]
            return False
        Just callback -> do
            st <- lift $ lift get
            lift $ callback p st
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
res_quit _ st = do
    logInfo $ formatS "Client #{} quit cleanly." [clientNumber st]
    writeServer Damn.disconnect
    return False

res_ping :: Callback
res_ping Packet { pktArguments = args } _ = do
    writeClient $ cmdPong args
    return True

res_mode :: Callback
res_mode p _ = do
    logInfo $ formatS "Received mode command, should handle: {}" [show p]
    return True

res_join :: Callback
res_join Packet { pktArguments = a } st = do
    case a of
        [] -> writeClient $ errNeedMoreParams (st ^. username)
        (rooms:_) -> forM_ (T.splitOn "," rooms) $ \r ->
            case toChatroom r of
                Nothing -> writeClient $ errNoSuchChannel (st ^. username) r
                Just c -> do
                    l <- getsState $ view loggedIn
                    if l
                       then writeServer =<< Damn.join c
                       else modifyState (joinList %~ S.insert c)
    return True

res_part :: Callback
res_part Packet { pktArguments = a } st = do
    case a of
        [] -> writeClient $ errNeedMoreParams (st ^. username)
        (room:_) -> case toChatroom room of
            Nothing -> writeClient $ errNoSuchChannel (st ^. username) room
            Just c -> writeServer =<< Damn.part c
    return True

res_privmsg :: Callback
res_privmsg Packet { pktArguments = (room:msg) } st = do
    case toChatroom room of
        Nothing -> writeClient $ errNoSuchChannel (st ^. username) room
        Just r -> case msg of
                      [] -> writeClient $ errNeedMoreParams (st ^. username)
                      (m:_) -> case T.stripPrefix "\1ACTION " m of
                                   Just ac -> writeServer =<< Damn.action r (T.init ac)
                                   Nothing -> writeServer =<< Damn.msg r m
    return True
res_privmsg _ st = writeClient (errNeedMoreParams (st ^. username)) >> return True

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
