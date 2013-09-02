{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Melvin.Types (
  ClientSettings(..),
  clientChan,
  serverChan,
  username,
  token,
  serverTid,
  clientTid,

  writeClient,
  writeServer
) where

import Control.Concurrent
import Control.Lens
import Control.Proxy
import Control.Proxy.Safe
import Control.Proxy.Trans.State
import Data.Text
import Prelude                   (Integer, ($))
import Melvin.Logger

data ClientSettings = ClientSettings
        { clientNumber :: Integer
        , _clientChan  :: Chan Text
        , _serverChan  :: Chan Text
        , _username    :: Text
        , _token       :: Text
        , _serverTid   :: ThreadId
        , _clientTid   :: ThreadId
        }

makeLenses ''ClientSettings

writeClient :: Proxy p => Text -> ExceptionP (StateP ClientSettings p) a' a b' b SafeIO ()
writeClient text = do
    logInfo text
    cc <- liftP $ gets (view clientChan)
    tryIO $ writeChan cc text

writeServer :: Proxy p => Text -> ExceptionP (StateP ClientSettings p) a' a b' b SafeIO ()
writeServer text = do
    logInfo text
    sc <- liftP $ gets (view serverChan)
    tryIO $ writeChan sc text
