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
  loggedIn,
  joinList,

  writeClient,
  writeServer,
  killClient,
  killServer,

  ClientP
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens
import Control.Proxy
import Control.Proxy.Safe
import Control.Proxy.Trans.State
import Data.Set
import Data.Text
import Melvin.Client.Packet
import Melvin.Logger
import Melvin.Prelude

type ClientP = ExceptionP (StateP ClientSettings ProxyFast)

data Chatroom =
          Chatroom Text
        | PrivateChat Text
        deriving (Eq, Ord)

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
        , _loggedIn             :: Bool
        , _joinList             :: Set Chatroom
        }

makeLenses ''ClientSettings

writeClient :: Packet -> ClientP a' a b' b SafeIO ()
writeClient text = do
    let r = render text
    logInfo r
    cc <- liftP $ gets (view clientChan)
    tryIO $ writeChan cc r

writeServer :: Text -> ClientP a' a b' b SafeIO ()
writeServer text = do
    logInfo $ show text
    sc <- liftP $ gets (view serverChan)
    tryIO $ writeChan sc (text ++ "\0")

killClient :: ClientP a' a b' b SafeIO ()
killClient = do
    ct <- liftP $ gets (view clientThreadId)
    tid <- tryIO $ tryTakeMVar ct
    case tid of
        Nothing -> logWarning "Client thread is already dead."
        Just t -> tryIO $ cancel t

killServer :: ClientP a' a b' b SafeIO ()
killServer = do
    ct <- liftP $ gets (view serverThreadId)
    tid <- tryIO $ tryTakeMVar ct
    case tid of
        Nothing -> logWarning "Server thread is already dead."
        Just t -> tryIO $ cancel t
