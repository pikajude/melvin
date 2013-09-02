{-# LANGUAGE DeriveDataTypeable #-}

module Melvin.Exception (
  MelvinException(..),
  isRetryable
) where

import           Control.Exception
import           Data.Text             (Text)
import           Data.Typeable
import           Melvin.Client.Packet  (Packet)
import           Melvin.Prelude
import qualified Text.Damn.Packet as D

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
      deriving (Show, Typeable)

instance Exception MelvinException

isRetryable :: SomeException -> Bool
isRetryable e | Just ServerDisconnect <- fromException e = True
isRetryable _ = False
