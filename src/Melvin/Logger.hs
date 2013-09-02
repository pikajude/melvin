{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

module Melvin.Logger (
  logInfo,
  logWarning,
  logError,

  logInfoIO,
  logWarningIO,
  logErrorIO
) where

import           Control.Monad.IO.Class
import           Control.Proxy
import           Control.Proxy.Safe
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time
import           Prelude hiding         (log)
import           System.Locale

data Level = Info | Warning | Error

logInfo, logWarning, logError :: Proxy p
                              => Text
                              -> ExceptionP p a' a b' b SafeIO ()
logInfo = log Info
logWarning = log Warning
logError = log Error

logInfoIO, logWarningIO, logErrorIO :: MonadIO m => Text -> m ()
logInfoIO = logIO Info
logWarningIO = logIO Warning
logErrorIO = logIO Error

log :: Proxy p => Level -> Text -> ExceptionP p a' a b' b SafeIO ()
log = logWith tryIO

logIO :: MonadIO m => Level -> Text -> m ()
logIO = logWith liftIO

logWith :: Monad m => (forall a. IO a -> m a) -> Level -> Text -> m ()
logWith f l s = do
    t <- f getZonedTime
    let prefix = case l of
            Info -> "---"
            Warning -> "\27[33m!!!\27[0m"
            Error -> "\27[31mERR\27[0m"
        fmt = "%F %X " ++ prefix ++ " "
        time = formatTime defaultTimeLocale fmt t
        clean = fromMaybe s $ T.stripSuffix "\n" s >>= T.stripSuffix "\r"
    f $ putStr time
    f $ T.putStrLn clean
