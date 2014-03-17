{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Melvin (
  doAMelvin
) where

import Control.Concurrent.Lifted
import Control.Concurrent.Async.Lifted
import Control.Monad
import Control.Monad.Fix
import Data.Set
import Melvin.Client as Client
import Melvin.Damn as Damn
import Melvin.Client.Auth
import Melvin.Exception
import Melvin.Prelude hiding    (index, set, utf8)
import Melvin.Options
import Melvin.Types
import Network
import System.IO

doAMelvin :: Mopts -> [String] -> IO ()
doAMelvin Mopts { moptPort = p
                , moptLogLevel = l
                , moptMaxClients = _
                } _args = runStdoutLoggingT l . (`evalStateT` error "no state") $ do
    $logInfo $ [st|Listening on %?.|] p
    server <- liftIO $ listenOn p
    forM_ [1..] $ \i -> do
        triple <- liftIO $ accept server
        async $ runClientPair i triple

runClientPair :: ClientT m => Integer -> (Handle, String, t) -> m ()
runClientPair index (h, host, _) = do
    $logInfo $ [st|Client #%? has connected from %s.|] index host
    liftIO $ hSetEncoding h utf8
    tokpair <- authenticate h
    case tokpair of
        Left e -> do
            $logWarn $ [st|Client #%? couldn't authenticate: %?.|] index e
            liftIO $ hClose h
        Right (uname, token_, rs) -> do
            buildClientSettings index h uname token_ rs

            -- | Run the server thread.
            --
            -- Uses Melvin.Exception's isRetryable to check whether errors
            -- are recoverable. If so, it increments the reconnect time by
            -- 5.
            server <- async $ do
                mt <- myThreadId
                sti <- use serverThreadId
                putMVar sti mt
                fix $ \f -> do
                    result <- try Damn.loop
                    case result of
                        r@Right{..} -> return r
                        Left e -> if isRetryable e
                            then retryWait += 5 >> f
                            else return $ Left e

            -- | Run the client thread.
            --
            -- This isn't retried, unlike dAmn, because if the client exits
            -- nobody really cares what happened on dAmn
            -- rec client <- async $ do
            client <- async $ do
                mt <- myThreadId
                cti <- use clientThreadId
                putMVar cti mt
                try $ Client.loop h

            result <- liftM2 (,) (wait client) (wait server)
            case result of
                (Right{..}, Right{..}) -> $logInfo $ [st|Client #%d exited cleanly|] index
                (Left m, _) -> $logWarn $ [st|Client #%d encountered an error: %?|] index (m :: SomeException)
                (_, Left m) -> $logWarn $ [st|Client #%d's server encountered an error: %?|] index m

buildClientSettings :: ClientT m => Integer -> Handle -> Text -> Text -> Set Chatroom -> m ()
buildClientSettings i h u t j = do
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
    put ClientSettings
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
