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
import Control.Monad
import Control.Monad.Fix
import Data.Set
import Melvin.Client as Client
import Melvin.Damn as Damn
import Melvin.Client.Auth
import Melvin.Exception
import Melvin.Logger
import Melvin.Prelude hiding    (index, set)
import Melvin.Options
import Melvin.Types
import Network
import System.Mem

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
        Right (uname, token_, rs) -> do
            set <- buildClientSettings index h uname token_ rs

            -- | Run the client thread.
            --
            -- This isn't retried, unlike dAmn, because if the client exits
            -- nobody really cares what happened on dAmn
            rec client <- async $ do
                 putMVar (set ^. clientThreadId) client
                 m <- runMelvin set $ Client.packetStream h >-> Client.responder
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
                         Damn.packetStream (set ^. serverMVar) >-> Damn.responder
                     performGC
                     case result of
                         r@Right{..} -> return r
                         Left err -> do
                             if isRetryable err
                                 then f (settings & retryWait +~ 5)
                                 else return $ Left err) set

            result <- liftM2 (,) (wait client) (wait server)
            case result of
                (Right{..}, Right{..}) -> logInfoIO $ formatS "Client #{} exited normally" [index]
                (Left m, _) -> logErrorIO $ "Client #" ++ show index
                                         ++ " encountered an error: " ++ show m
                (_, Left m) -> logErrorIO $ "Client #" ++ show index
                                         ++ "'s server encountered an error: " ++ show m

buildClientSettings :: Integer -> Handle -> Text -> Text -> Set Chatroom -> IO ClientSettings
buildClientSettings i h u t j = do
    sc <- newMVar ()
    cc <- newMVar ()
    mv1 <- newEmptyMVar
    mv2 <- newEmptyMVar
    mv3 <- newEmptyMVar
    csm <- newMVar ClientState
             { _loggedIn    = False
             , _joinList    = j
             , _privclasses = mempty
             , _users       = mempty
             }
    return ClientSettings
        { clientNumber     = i
        , _clientHandle    = h
        , _serverWriteLock = sc
        , _clientWriteLock = cc
        , _username        = u
        , _token           = t
        , _serverMVar      = mv1
        , _serverThreadId  = mv2
        , _clientThreadId  = mv3
        , _retryWait       = 5
        , _clientState     = csm
        }

insertDamn :: ClientSettings -> IO ()
insertDamn ClientSettings { _serverMVar = m } = do
    void $ tryTakeMVar m
    h <- connectTo "chat.deviantart.com" (PortNumber 3900)
    putMVar m h
