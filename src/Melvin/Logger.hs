{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

module Melvin.Logger (
  startLogger,

  logInfo,
  logWarning,
  logError,

  logInfoIO,
  logWarningIO,
  logErrorIO
) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Proxy
import           Control.Proxy.Safe
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time
import           Melvin.Prelude hiding  (log)
import           System.Locale
import           System.IO.Unsafe

logChan :: Chan Text
logChan = unsafePerformIO newChan
{-# NOINLINE logChan #-}

startLogger :: IO ()
startLogger = void $ forkIO $ forever $ readChan logChan >>= T.putStrLn

data Level = Info | Warning | Error

logInfo, logWarning, logError :: Proxy p => Text -> ExceptionP p a' a b' b SafeIO ()
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
        clean = T.stripEnd $ fromMaybe s $ T.stripSuffix "\0" s
        str = pack time <> clean
    f $ writeChan logChan str
