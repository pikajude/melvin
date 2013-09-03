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
import           Melvin.Client.Packet
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
                       ]

res_ping :: Callback
res_ping Packet { pktArguments = a } _ = do
    writeClient $ Packet Nothing "PONG" a
    return True

res_quit :: Callback
res_quit _ st = do
    logInfo $ formatS "Client #{} quit cleanly." [clientNumber st]
    writeServer "disconnect\n"
    return False

res_mode :: Callback
res_mode p _ = do
    logInfo $ formatS "Received mode command, should handle: {}" [show p]
    return True

res_join :: Callback
res_join Packet { pktArguments = a } st = do
    case a of
        [] -> writeClient $ errNeedMoreParams (st ^. username)
        _rooms -> return ()
    return True

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
