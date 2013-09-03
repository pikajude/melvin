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

handler :: Proxy p
        => SomeException
        -> ExceptionP (StateP ClientSettings p) a' a b' b SafeIO ()
handler ex = do
    killServer
    throw ex

packetStream :: Proxy p
             => Integer -> Handle -> ()
             -> Producer (ExceptionP (StateP ClientSettings p)) Packet SafeIO ()
packetStream index hndl () = bracket id
    (return hndl)
    (\h -> do
        logInfoIO $ "Client #" ++ show index ++ " has disconnected."
        hClose h)
    (\h -> handle handler $ fix $ \f -> do
        isEOF <- tryIO $ hIsEOF h
        isClosed <- tryIO $ hIsClosed h
        unless (isEOF || isClosed) $ do
            line <- tryIO $ hGetLine h
            logInfo line
            respond $ parse line
            f)

responder :: Proxy p
          => () -> Consumer (ExceptionP (StateP ClientSettings p)) Packet SafeIO ()
responder () = fix $ \f -> do
    p <- request ()
    case M.lookup (pktCommand p) responses of
        Nothing -> logInfo $ formatS "Unhandled packet from client: {}" [show p]
        Just callback -> callback p
    unless (pktCommand p == "QUIT") f


-- | Big ol' list of callbacks!
type Callback p = Packet -> Consumer (ExceptionP (StateP ClientSettings p)) Packet SafeIO ()

responses :: Proxy p => M.Map Text (Callback p)
responses = M.fromList [ ("PING", res_ping)
                       , ("QUIT", res_quit)
                       , ("USER", const (return ()))
                       ]

res_ping :: Proxy p => Callback p
res_ping Packet { pktArguments = a } =
        writeClient $ Packet Nothing "PONG" a

res_quit :: Proxy p => Callback p
res_quit _ = do
    num <- liftP $ gets clientNumber
    logInfo $ formatS "Client #{} quit cleanly." [num]

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket" :: String) #-}
