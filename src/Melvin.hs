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
            put =<< buildClientSettings index h uname token_ rs

            result <- concurrently (runClient h) runServer

            case result of
                (Right{..}, Right{..}) -> $logInfo $ [st|Client #%d exited cleanly|] index
                (Left m, _) -> $logWarn $ [st|Client #%d encountered an error: %?|] index m
                (_, Left m) -> $logWarn $ [st|Client #%d's server encountered an error: %?|] index m

-- This isn't retried because nobody cares what happened on dAmn if the
-- client quits.
runClient :: ClientT m => Handle -> m (Either SomeException ())
runClient h = do
    mt <- myThreadId
    ct <- use clientThreadId
    putMVar ct mt
    try $ Client.loop h

runServer :: ClientT m => m (Either SomeException ())
runServer = do
    mt <- myThreadId
    st_ <- use serverThreadId
    putMVar st_ mt
    fix $ \f -> do
        result <- try Damn.loop
        case result of
            r@Right{..} -> return r
            Left e -> if isRetryable e
                then retryWait += 5 >> f
                else return $ Left e

buildClientSettings :: ClientT m => Integer -> Handle -> Text -> Text -> Set Chatroom -> m ClientSettings
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
