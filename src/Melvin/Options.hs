module Melvin.Options (
  Mopts(..),
  module Options
) where

import Control.Applicative
import Control.Monad.Logger
import Data.Char
import Data.Word
import Network
import Options
import Prelude

data Mopts = Mopts
    { moptPort       :: PortID
    , moptLogLevel   :: LogLevel
    , moptMaxClients :: Maybe Word8
    }

levelParser :: String -> Either String LogLevel
levelParser "debug" = Right LevelDebug
levelParser "info" = Right LevelInfo
levelParser "warn" = Right LevelWarn
levelParser "error" = Right LevelError
levelParser x = Left $ "Unknown log level " ++ x

levelShower :: LogLevel -> String
levelShower = map toLower . drop 5 . show

instance Options Mopts where
    defineOptions = Mopts . PortNumber . toEnum . fromEnum

        <$> defineOption optionType_word16 (\o -> o
            { optionLongFlags = ["port"]
            , optionShortFlags = "p"
            , optionDefault = 6667
            , optionDescription = "Port to listen on." })

        <*> defineOption (optionType "log level" LevelWarn levelParser levelShower) (\o -> o
            { optionLongFlags = ["log-level"]
            , optionShortFlags = "l"
            , optionDefault = LevelWarn
            , optionDescription = "Minimum severity of messages to print. One of debug, info, warn, error." })

        <*> defineOption (optionType_maybe optionType_word8) (\o -> o
            { optionLongFlags = ["max-clients"]
            , optionShortFlags = "c"
            , optionDefault = Nothing
            , optionDescription = "Maximum number of clients to allow." })
