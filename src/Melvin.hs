{-# LANGUAGE RecordWildCards #-}

module Melvin (
  doAMelvin
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens hiding      (index, set)
import Control.Monad
import Control.Proxy.Concurrent
import Control.Proxy.Safe
import Control.Proxy
import Melvin.Client as Client
import Melvin.Client.Auth
import Melvin.Logger
import Melvin.Prelude
import Melvin.Options
import Melvin.Types
import Network

doAMelvin :: Options -> IO ()
doAMelvin Options { optionPort = p
                  , optionMaxClients = _
                  } = do
    server <- listenOn p
    forM_ [1..] $ \i -> do
        triple <- accept server
        async $ do
            runClientPair i triple
            performGC

runClientPair :: Integer -> (Handle, String, t) -> IO ()
runClientPair index (h, host, _) = do
    logInfoIO $ "Client #" ++ show index ++ " has connected from " ++ pack host
    tokpair <- authenticate h
    case tokpair of
        Left err -> do
            logWarningIO $ "Client #" ++ show index ++ " couldn't authenticate: " ++ show err
            hClose h
        Right (uname, token_) -> do
            set <- buildClientSettings index h uname token_
            client <- async $ do
                m <- runMelvin set $ Client.packetStream index h >-> Client.responder
                performGC
                return m
            server <- async $ do
                performGC
                return (Right () :: Either SomeException ())
            result <- liftM2 (,) (wait client) (wait server)
            killThread $ set ^. serverTid
            killThread $ set ^. clientTid
            case result of
                (Right{..}, Right{..}) -> logInfoIO $ "Client #" ++ show index ++ " exited normally"
                (Left m, _) -> logErrorIO $ "Client #" ++ show index
                                         ++ " exited unexpectedly: " ++ show m
                (_, Left m) -> logErrorIO $ "Client #" ++ show index
                                         ++ "'s server disconnected unexpectedly: " ++ show m

buildClientSettings :: Integer -> Handle -> Text -> Text -> IO ClientSettings
buildClientSettings i h u t = do
    sc <- newChan
    cc <- newChan
    serverH <- connectTo "chat.deviantart.com" (PortNumber 3900)
    sid <- forkIO $ forever $ readChan sc >>= hPutStr serverH
    cid <- forkIO $ forever $ readChan cc >>= hPutStr h
    return ClientSettings
        { clientNumber = i
        , _serverChan  = sc
        , _clientChan  = cc
        , _username    = u
        , _token       = t
        , _serverTid   = sid
        , _clientTid   = cid
        }
