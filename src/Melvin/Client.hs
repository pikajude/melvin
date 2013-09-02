module Melvin.Client (
  packetStream,
  responder
) where

import Control.Monad
import Control.Monad.Fix
import Control.Proxy
import Control.Proxy.Safe
import Control.Proxy.Trans.State
import Data.Maybe
import Melvin.Client.Packet
import Melvin.Logger
import Melvin.Prelude
import Melvin.Types

packetStream :: Proxy p
             => Integer -> Handle -> ()
             -> Producer (ExceptionP (StateP ClientSettings p)) Packet SafeIO ()
packetStream index hndl () = bracket id
    (return hndl)
    (\h -> do
        logInfoIO $ "Client #" ++ show index ++ " has disconnected."
        hClose h)
    (\h -> fix $ \f -> do
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
    p@Packet { pktPrefix = _prefix
             , pktCommand = command
             , pktArguments = args } <- request ()
    logInfo (show p)
    case command of
        "PING" -> writeClient $ formatS "PONG {}" args
        "QUIT" -> do
            num <- liftP $ gets clientNumber
            logInfo $ formatS "Client #{} quitting ({})." [show num, fromMaybe "no reason" $ listToMaybe args]
        _ -> return ()
    unless (command == "QUIT") f
