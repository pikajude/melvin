{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Melvin uses both I/O functions and Text heavily. This module provides
-- everything a Melvin module should need.
module Melvin.Prelude (
  -- base's Prelude
  module X,
  st,
  stP,
  sb,
  sbP,
  LogIO,
  runStdoutLoggingT,

  -- retconned Prelude functions
  (++),
  show,
  (<$>),
  Category,

  -- IO
  runMelvin,
  reading,
  splittingBy,
  pass,

  IO.hGetLine,
  IO.putStrLn,
  IO.hPutStr,
  IO.ByteString,

  -- Text
  Text,
  pack,
  binary,
  utf8
) where

import           Control.Applicative
import           Control.Category                (Category)
import           Control.Exception               (IOException)
import           Control.Exception as X          (AsyncException(..))
import           Control.Lens as X hiding        (Level)
import           Control.Monad.Catch as X hiding (bracket, bracket_)
import           Control.Monad.IO.Class
import           Control.Monad.Logger as X hiding (runStdoutLoggingT)
import           Control.Monad.State as X hiding (join)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as IO
import           Data.Machine as X               (MachineT, PlanT, await, repeatedly, runT_, auto, construct, stop, yield, (~>))
import           Data.Monoid as X
import           Data.Text                       (Text, pack)
import           Data.Text.Encoding              (decodeUtf8, encodeUtf8)
import           FileLocation as X
import           Melvin.Exception
import           Melvin.Internal.Orphans as X    ()
import           Melvin.Internal.MonadAsync as X
import           Prelude as X hiding             ((++), putStrLn, print, show, lines)
import qualified Prelude as P
import           System.IO as X                  (Handle, hClose, hFlush, hIsClosed, hIsEOF)
import           System.IO                       (stdout)
import           System.Log.FastLogger           (fromLogStr)
import           Text.Printf.TH

type LogIO m = (MonadLogger m, MonadIO m)

binary :: Text -> S8.ByteString
binary = encodeUtf8

utf8 :: S8.ByteString -> Text
utf8 = decodeUtf8

-- | Simple utility functions.
show :: Show a => a -> Text
show = pack . P.show

(++) :: Monoid m => m -> m -> m
(++) = (<>)

pass :: (Monad m, Category k) => (o -> PlanT (k o) o m t) -> MachineT m (k o) o
pass f = repeatedly $ do
    m <- await
    _ <- f m
    yield m

reading :: MonadIO m => Handle -> Int -> MachineT m k IO.ByteString
reading h n = repeatedly $ yield =<< liftIO (IO.hGetSome h n)

splittingBy :: (Monad m, Category k) => IO.ByteString -> MachineT m (k IO.ByteString) IO.ByteString
splittingBy sep = repeatedly $ go "" where
    waitNext str = if IO.null b
                       then go a
                       else yield a >> go (IO.drop (IO.length sep) b)
        where (a, b) = IO.breakSubstring sep str
    go buffer = if sep `IO.isInfixOf` buffer
                    then waitNext buffer
                    else do
                        str <- await <|> (unless (IO.null buffer) (yield buffer) *> stop)
                        waitNext (buffer <> str)

-- redefined. monad-logger's function likes to output an extra newline.
runStdoutLoggingT :: LogLevel -> LoggingT m a -> m a
runStdoutLoggingT baseLevel = (`runLoggingT` defaultOutput stdout) where
    defaultOutput h loc src level msg =
        when (level >= baseLevel) (S8.hPutStr h ls) where
            ls = fromLogStr $ defaultLogStr loc src level msg

-- | Dealing with the underlying Proxy monad upon which Melvin clients are
-- based. Despite the type signature, this function only handles IO
-- exceptions and Melvin-internal exceptions.
runMelvin :: (MonadIO m, MonadCatch m) => s -> StateT s m r -> m (Either SomeException r)
runMelvin st_ m = catches
    (liftM Right $ evalStateT m st_)
    [ Handler $ \(e :: IOException) -> return (Left $ toException e)
    , Handler $ \(e :: MelvinException) -> return (Left $ toException e)
    ]
