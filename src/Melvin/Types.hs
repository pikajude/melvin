{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Melvin.Types (
  ClientSettings(..),
  clientChan,
  serverChan,
  username,
  token,
  clientWriterThreadId,
  serverWriterThreadId,
  serverMVar,
  clientThreadId,
  serverThreadId,
  retryWait,

  writeClient,
  writeServer,
  killClient,
  killServer
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens
import Control.Proxy
import Control.Proxy.Safe
import Control.Proxy.Trans.State
import Data.Text
import Melvin.Logger
import Melvin.Prelude

data ClientSettings = ClientSettings
        { clientNumber          :: Integer
        , _clientChan           :: Chan Text
        , _serverChan           :: Chan Text
        , _username             :: Text
        , _token                :: Text
        , _clientWriterThreadId :: ThreadId
        , _serverWriterThreadId :: ThreadId
        , _serverMVar           :: MVar Handle
        , _clientThreadId       :: MVar (Async (Either SomeException ()))
        , _serverThreadId       :: MVar (Async (Either SomeException ()))
        , _retryWait            :: Integer
        }

makeLenses ''ClientSettings

writeClient :: Proxy p => Text -> ExceptionP (StateP ClientSettings p) a' a b' b SafeIO ()
writeClient text = do
    logInfo $ show text
    cc <- liftP $ gets (view clientChan)
    tryIO $ writeChan cc text

writeServer :: Proxy p => Text -> ExceptionP (StateP ClientSettings p) a' a b' b SafeIO ()
writeServer text = do
    logInfo $ show text
    sc <- liftP $ gets (view serverChan)
    tryIO $ writeChan sc text

killClient :: Proxy p => ExceptionP (StateP ClientSettings p) a' a b' b SafeIO ()
killClient = do
    ct <- liftP $ gets (view clientThreadId)
    tid <- tryIO $ tryTakeMVar ct
    case tid of
        Nothing -> logWarning "Client thread is already dead."
        Just t -> tryIO $ cancel t

killServer :: Proxy p => ExceptionP (StateP ClientSettings p) a' a b' b SafeIO ()
killServer = do
    ct <- liftP $ gets (view serverThreadId)
    tid <- tryIO $ tryTakeMVar ct
    case tid of
        Nothing -> logWarning "Server thread is already dead."
        Just t -> tryIO $ cancel t
