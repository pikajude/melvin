module Melvin.Client (
  packetStream,
  responder
) where

import           Control.Monad
import           Control.Monad.Fix
import           Control.Proxy
import           Control.Proxy.Safe
import           Control.Proxy.Trans.State
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           Melvin.Chatrooms
import           Melvin.Client.Packet hiding (render)
import qualified Melvin.Damn.Actions as Damn
import           Melvin.Logger
import           Melvin.Prelude
import           Melvin.Types

handler :: SomeException -> ClientP a' a b' b SafeIO ()
handler ex = do
    logError $ show ex
    killServer
    throw ex

packetStream :: Handle -> () -> Producer ClientP Packet SafeIO ()
packetStream hndl () = bracket id
    (return hndl)
    hClose
    (\h -> handle handler $ fix $ \f -> do
        isEOF <- tryIO $ hIsEOF h
        isClosed <- tryIO $ hIsClosed h
        unless (isEOF || isClosed) $ do
            line <- tryIO $ hGetLine h
            logInfo line
            respond $ parse line
            f)

responder :: () -> Consumer ClientP Packet SafeIO ()
responder () = handle handler $ fix $ \f -> do
    p <- request ()
    continue <- case M.lookup (pktCommand p) responses of
        Nothing -> do
            logInfo $ formatS "Unhandled packet from client: {}" [show p]
            return False
        Just callback -> do
            st <- liftP get
            callback p st
    when continue f


-- | Big ol' list of callbacks!
type Callback = Packet -> ClientSettings -> Consumer ClientP Packet SafeIO Bool

responses :: M.Map Text Callback
responses = M.fromList [ ("PING", res_ping)
                       , ("QUIT", res_quit)
                       , ("USER", \_ _ -> return True)
                       , ("MODE", res_mode)
                       , ("JOIN", res_join)
                       , ("PRIVMSG", res_privmsg)
                       ]

res_ping :: Callback
res_ping Packet { pktArguments = a } _ = do
    writeClient $ Packet Nothing "PONG" a
    return True

res_quit :: Callback
res_quit _ st = do
    logInfo $ formatS "Client #{} quit cleanly." [clientNumber st]
    writeServer Damn.disconnect
    return False

res_mode :: Callback
res_mode p _ = do
    logInfo $ formatS "Received mode command, should handle: {}" [show p]
    return True

res_join :: Callback
res_join Packet { pktArguments = a } st = do
    case a of
        [] -> writeClient $ errNeedMoreParams (st ^. username)
        rooms -> forM_ rooms $ \r ->
            case toChatroom r of
                Nothing -> writeClient $ errNoSuchChannel (st ^. username) r
                Just c -> do
                    l <- getsState $ view loggedIn
                    if l
                       then writeServer =<< Damn.join c
                       else modifyState (joinList %~ S.insert c)
    return True

res_privmsg :: Callback
res_privmsg Packet { pktArguments = (room:msg) } st = do
    case toChatroom room of
        Nothing -> writeClient $ errNoSuchChannel (st ^. username) room
        Just r -> case msg of
                      [] -> writeClient $ errNeedMoreParams (st ^. username)
                      (m:_) -> writeServer =<< Damn.msg r m
    return True
res_privmsg _ st = writeClient (errNeedMoreParams (st ^. username)) >> return True

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
