{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Melvin.Exception (
  MelvinException(..),
  isRetryable
) where

import Control.Exception
import Data.Text            (unpack)
import Data.Typeable
import Melvin.Prelude
import Prelude              (Show(..))

data MelvinException =
        -- | Client sent something that doesn't parse.
        ClientNoParse Text
        -- | Server sent something that doesn't parse.
      | ServerNoParse String Text
        -- | Server disconnected unexpectedly.
      | ServerDisconnect
        -- | Authentication with dAmn failed, despite the fact that dAmn
        -- returned this authtoken. Weird.
      | AuthenticationFailed Text
      deriving Typeable

instance Exception MelvinException

instance Show MelvinException where
    show (AuthenticationFailed e) = "couldn't login: " ++ unpack e
    show ServerDisconnect = "lost connection to server"
    show ServerNoParse{..} = "received a bad packet from the server"
    show ClientNoParse{..} = "received a bad packet from the client"

isRetryable :: SomeException -> Bool
isRetryable e | Just ServerDisconnect <- fromException e = True
isRetryable _ = False
