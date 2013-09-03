{-# LANGUAGE DeriveDataTypeable #-}

module Melvin.Exception (
  MelvinException(..),
  isRetryable
) where

import Control.Exception
import Data.Typeable
import Melvin.Client.Packet (Packet)
import Melvin.Prelude

data MelvinException =
        -- | Client packets that are syntactically valid, but are lacking
        -- elements the program expects them to have.
        InvalidClientPacket Packet
        -- | Client sent something that doesn't parse.
      | ClientNoParse Text
        -- | Server sent something that doesn't parse.
      | ServerNoParse String Text
        -- | Server disconnected unexpectedly.
      | ServerDisconnect
        -- | Authentication with dAmn failed, despite the fact that dAmn
        -- returned this authtoken. Weird.
      | AuthenticationFailed
      deriving (Show, Typeable)

instance Exception MelvinException

isRetryable :: SomeException -> Bool
isRetryable e | Just ServerDisconnect <- fromException e = True
isRetryable _ = False
