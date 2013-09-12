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
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Thyme
import           Melvin.Prelude hiding  (log)
import           Pipes.Safe
import           System.Locale
import           System.IO
import           System.IO.Unsafe

logChan :: Chan Text
logChan = unsafePerformIO newChan
{-# NOINLINE logChan #-}

startLogger :: IO ()
startLogger = do
    hSetBuffering stdout LineBuffering
    void $ forkIO $ forever $ readChan logChan >>= T.putStrLn

data Level = Info | Warning | Error

logInfo, logWarning, logError :: Text -> SafeT (StateT s IO) ()
logInfo    = log Info
logWarning = log Warning
logError   = log Error

logInfoIO, logWarningIO, logErrorIO :: MonadIO m => Text -> m ()
logInfoIO = logIO Info
logWarningIO = logIO Warning
logErrorIO = logIO Error

log :: Level -> Text -> SafeT (StateT s IO) ()
log = logWith (lift . lift)

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
