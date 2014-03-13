{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Melvin (
  doAMelvin
) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Data.Set
import Melvin.Client as Client
import Melvin.Damn as Damn
import Melvin.Client.Auth
import Melvin.Exception
import Melvin.Prelude hiding    (index, set)
import Melvin.Options
import Melvin.Types
import Network
import System.IO
import System.Mem

doAMelvin :: Mopts -> [String] -> IO ()
doAMelvin Mopts { moptPort = p
                , moptMaxClients = _
                } _args = runStdoutLoggingT $ do
    $logInfo $ [st|Listening on %?.|] p
    server <- liftIO $ listenOn p
    forM_ [1..] $ \i -> do
        triple <- liftIO $ accept server
        async $ do
            runClientPair i triple
            liftIO performGC

runClientPair :: Integer -> (Handle, String, t) -> LoggingT IO ()
runClientPair index (h, host, _) = do
    $logInfo $ [st|Client #%? has connected from %s.|] index host
    liftIO $ hSetEncoding h utf8
    tokpair <- authenticate h
    case tokpair of
        Left e -> do
            $logWarn $ [st|Client #%? couldn't authenticate: %?.|] index e
            liftIO $ hClose h
        Right (uname, token_, rs) -> do
            set <- buildClientSettings index h uname token_ rs

            -- | Run the client thread.
            --
            -- This isn't retried, unlike dAmn, because if the client exits
            -- nobody really cares what happened on dAmn
            rec client <- async $ do
                 liftIO $ putMVar (set ^. clientThreadId) client
                 m <- runMelvin set $ Client.packetStream h >-> Client.responder
                 liftIO performGC
                 return m

            -- | Run the server thread.
            --
            -- Uses Melvin.Exception's isRetryable to check whether errors
            -- are recoverable. If so, it increments the reconnect time by
            -- 5.
            rec server <- async $ do
                 liftIO $ putMVar (set ^. serverThreadId) server
                 fix (\f settings -> do
                     insertDamn settings
                     result <- runMelvin settings $
                         Damn.packetStream (set ^. serverMVar) >-> Damn.responder
                     liftIO performGC
                     case result of
                         r@Right{..} -> return r
                         Left e -> if isRetryable e
                             then f (settings & retryWait +~ 5)
                             else return $ Left e) set

            result <- liftM2 (,) (wait client) (wait server)
            case result of
                (Right{..}, Right{..}) -> $logInfo $ [st|Client #%d exited cleanly|] index
                (Left m, _) -> $logWarn $ [st|Client #%d encountered an error: %?|] index m
                (_, Left m) -> $logWarn $ [st|Client #%d's server encountered an error: %?|] index m

buildClientSettings :: LogIO m => Integer -> Handle -> Text -> Text -> Set Chatroom -> m ClientSettings
buildClientSettings i h u t j = liftIO $ do
    sc <- newMVar ()
    cc <- newMVar ()
    mv1 <- newEmptyMVar
    mv2 <- newEmptyMVar
    mv3 <- newEmptyMVar
    csm <- newMVar ClientState
             { _loggedIn    = False
             , _joinList    = j
             , _joining     = mempty
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

insertDamn :: LogIO m => ClientSettings -> m ()
insertDamn ClientSettings { _serverMVar = m } = liftIO $ do
    void $ tryTakeMVar m
    h <- connectTo "chat.deviantart.com" (PortNumber 3900)
    putMVar m h
