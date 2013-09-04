{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Melvin uses both I/O functions and Text heavily. This module provides
-- everything a Melvin module should need.
module Melvin.Prelude (
  -- base's Prelude
  module X,
  formatS,

  -- retconned Prelude functions
  (++),
  show,
  (<$>),

  -- IO
  runMelvin,

  IO.hGetLine,
  IO.putStrLn,
  IO.hPutStr,
  io,

  -- Text
  Text,
  pack
) where

import           Control.Applicative
import           Control.Lens as X hiding  (Level)
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Proxy
import           Control.Proxy.Safe
import           Control.Proxy.Trans.State
import           Data.Monoid as X
import           Data.Text                 (Text, pack)
import           Data.Text.Format as X
import           Data.Text.Format.Params   (Params)
import qualified Data.Text.IO as IO
import           Data.Text.Lazy            (toStrict)
import           Prelude as X hiding       ((++), putStrLn, print, show)
import qualified Prelude as P
import           System.IO as X            (Handle, hClose, hFlush, hIsClosed, hIsEOF)

-- | Simple utility functions.
show :: Show a => a -> Text
show = pack . P.show

(++) :: Monoid m => m -> m -> m
(++) = (<>)

io :: MonadIO m => IO a -> m a
io = liftIO

-- | Convert the result of a 'format' call to strict Text.
formatS :: Params ps => Format -> ps -> Text
formatS a b = toStrict $ format a b

-- | Dealing with the underlying Proxy monad upon which Melvin clients are
-- based.
runMelvin :: s -> (() -> ExceptionP (StateP s ProxyFast) a' () () b SafeIO r) -> IO (Either SomeException r)
runMelvin st = trySafeIO . runProxy . evalStateK st . runEitherK
