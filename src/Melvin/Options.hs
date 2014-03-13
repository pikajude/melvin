module Melvin.Options (
  Mopts(..),
  module Options
) where

import Control.Applicative
import Data.Word
import Network
import Options
import Prelude

data Mopts = Mopts
    { moptPort       :: PortID
    , moptMaxClients :: Maybe Word8
    }

instance Options Mopts where
    defineOptions = Mopts . PortNumber . toEnum . fromEnum
        <$> defineOption optionType_word16 (\o -> o
            { optionLongFlags = ["port"]
            , optionShortFlags = "p"
            , optionDefault = 6667
            , optionDescription = "Port to listen on" })
        <*> defineOption (optionType_maybe optionType_word8) (\o -> o
            { optionLongFlags = ["max-clients"]
            , optionShortFlags = "c"
            , optionDefault = Nothing
            , optionDescription = "Maximum number of clients to allow" })
