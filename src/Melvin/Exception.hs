{-# LANGUAGE DeriveDataTypeable #-}

module Melvin.Exception (
    MelvinException(..)
) where

import Control.Exception
import Data.Text            (Text)
import Data.Typeable
import Melvin.Client.Packet (Packet)
import Prelude              (Show)

data MelvinException =
        -- | Client packets that are syntactically valid, but are lacking
        -- elements the program expects them to have.
        InvalidClientPacket Packet
        -- | Client sent something that doesn't parse.
      | ClientNoParse Text
      deriving (Show, Typeable)

instance Exception MelvinException
