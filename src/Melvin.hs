{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 704
{-# LANGUAGE DoRec #-}
#else
{-# LANGUAGE RecursiveDo #-}
#endif

module Melvin (
  doAMelvin
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens hiding      (index, set)
import Control.Monad
import Control.Monad.Fix
import Control.Proxy.Concurrent
import Control.Proxy
import Melvin.Client as Client
import Melvin.Damn as Damn
import Melvin.Client.Auth
import Melvin.Exception
import Melvin.Logger
import Melvin.Prelude
import Melvin.Options
import Melvin.Types
import Network

doAMelvin :: Options -> IO ()
doAMelvin Options { optionPort = p
                  , optionMaxClients = _
                  } = do
    startLogger
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

            -- | Run the client thread.
            --
            -- No error catching here because if the client exits, hu kars
            -- what happened on dAmn?
            rec client <- async $ do
                    putMVar (set ^. clientThreadId) client
                    m <- runMelvin set $ Client.packetStream index h >-> Client.responder
                    performGC
                    return m

            -- | Run the server thread.
            --
            -- Uses Melvin.Exception's isRetryable to check whether errors
            -- are recoverable. If so, it increments the reconnect time by
            -- 5.
            rec server <- async $ do
                 putMVar (set ^. serverThreadId) server
                 fix (\f settings -> do
                     insertDamn settings
                     result <- runMelvin settings $
                         Damn.packetStream index (set ^. serverMVar) >-> Damn.responder
                     performGC
                     case result of
                         r@Right{..} -> return r
                         Left err -> do
                            logErrorIO $ formatS "{}" [show err]
                            if isRetryable err
                                then f (settings & retryWait +~ 5)
                                else return $ Left err) set

            result <- liftM2 (,) (wait client) (wait server)
            killThread $ set ^. serverWriterThreadId
            killThread $ set ^. clientWriterThreadId
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
    mv1 <- newEmptyMVar
    mv2 <- newEmptyMVar
    mv3 <- newEmptyMVar
    sid <- forkIO $ forever $ do
        val <- readChan sc
        servar <- readMVar mv1
        hPutStr servar val
    cid <- forkIO $ forever $ readChan cc >>= hPutStr h
    return ClientSettings
        { clientNumber          = i
        , _serverChan           = sc
        , _clientChan           = cc
        , _username             = u
        , _token                = t
        , _serverWriterThreadId = sid
        , _clientWriterThreadId = cid
        , _serverMVar           = mv1
        , _serverThreadId       = mv2
        , _clientThreadId       = mv3
        , _retryWait            = 5
        }

insertDamn :: ClientSettings -> IO ()
insertDamn ClientSettings { _serverMVar = m } = do
    void $ tryTakeMVar m
    h <- connectTo "chat.deviantart.com" (PortNumber 3900)
    putMVar m h
