{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Melvin.Types (
  ClientSettings(..),
  clientWriteLock,
  serverWriteLock,
  username,
  token,
  serverMVar,
  clientThreadId,
  serverThreadId,
  retryWait,

  writeClient,
  writeServer,
  killClient,
  killServer,

  ClientState(..),
  loggedIn,
  joinList,

  modifyState,
  getState,
  getsState,
  putState,

  ClientP
) where

import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Control.Exception as E
import           Control.Proxy
import           Control.Proxy.Safe
import           Control.Proxy.Trans.State
import           Data.Set
import           Data.Text
import           Melvin.Client.Packet
import           Melvin.Exception
import           Melvin.Logger
import           Melvin.Prelude

data Chatroom =
          Chatroom Text
        | PrivateChat Text
        deriving (Eq, Ord)

data ClientState = ClientState
        { _loggedIn :: Bool
        , _joinList :: Set Chatroom
        }

makeLenses ''ClientState

data ClientSettings = ClientSettings
        { clientNumber          :: Integer
        , _clientHandle         :: Handle
        , _clientWriteLock      :: MVar ()
        , _serverWriteLock      :: MVar ()
        , _username             :: Text
        , _token                :: Text
        , _serverMVar           :: MVar Handle
        , _clientThreadId       :: MVar (Async (Either SomeException ()))
        , _serverThreadId       :: MVar (Async (Either SomeException ()))
        , _retryWait            :: Integer
        , _clientState          :: MVar ClientState
        }

makeLenses ''ClientSettings

type ClientP = ExceptionP (StateP ClientSettings ProxyFast)

writeClient :: Packet -> ClientP a' a b' b SafeIO ()
writeClient text = do
    (mv, h) <- liftP $ gets (view clientWriteLock &&& view clientHandle)
    tryIO $ E.bracket_
        (takeMVar mv)
        (putMVar mv ())
        ((hPutStr h r >> logInfoIO r) `E.catch` fallback)
    where fallback e = do
            logWarningIO $ "(write failed) " ++ r
            E.throw $ ClientSocketErr e
          r = render text

writeServer :: Text -> ClientP a' a b' b SafeIO ()
writeServer text = do
    (mv, h) <- liftP $ gets (view serverWriteLock &&& view serverMVar)
    tryIO $ E.bracket
        (takeMVar mv >> readMVar h)
        (\_ -> putMVar mv ())
        (\hndl -> (hPutStr hndl (text ++ "\0") >> logInfoIO (show text)) `E.catch` fallback)
    where fallback e = do
            logWarningIO $ "(write failed) " ++ show text
            E.throw $ ServerSocketErr e

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

modifyState :: (ClientState -> ClientState) -> ClientP a' a b' b SafeIO ()
modifyState f = do
    cs <- liftP $ gets (view clientState)
    tryIO $ modifyMVar_ cs (return . f)

getState :: ClientP a' a b' b SafeIO ClientState
getState = do
    cs <- liftP $ gets (view clientState)
    tryIO $ readMVar cs

getsState :: (ClientState -> a) -> ClientP a' a b' b SafeIO a
getsState f = do
    cs <- liftP $ gets (view clientState)
    st <- tryIO $ readMVar cs
    return $ f st

putState :: ClientState -> ClientP a' a b' b SafeIO ()
putState v = do
    cs <- liftP $ gets (view clientState)
    _ <- tryIO $ tryTakeMVar cs
    tryIO $ putMVar cs v
