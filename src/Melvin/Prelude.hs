{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
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
  error,

  -- IO
  runMelvin,

  IO.hGetLine,
  IO.putStrLn,
  IO.hPutStr,

  -- Text
  Text,
  pack
) where

import           Control.Applicative
import           Control.Lens as X hiding        (Level)
import           Control.Monad.IO.Class
import           Control.Monad.State as X hiding (join)
import           Data.Monoid as X
import           Data.Text                       (Text, pack)
import           Data.Text.Format as X
import           Data.Text.Format.Params         (Params)
import qualified Data.Text.IO as IO
import           Data.Text.Lazy                  (toStrict)
import           Melvin.Prelude.TH
import           Pipes as X hiding               (each, (<~))
import           Pipes.Safe
import           Prelude as X hiding             ((++), error, putStrLn, print, show, lines
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 704
                                                 , catch
#endif
                                                 )
import qualified Prelude as P
import           System.IO as X                  (Handle, hClose, hFlush, hIsClosed, hIsEOF)

-- | Simple utility functions.
show :: Show a => a -> Text
show = pack . P.show

(++) :: Monoid m => m -> m -> m
(++) = (<>)

-- | Convert the result of a 'format' call to strict Text.
formatS :: Params ps => Format -> ps -> Text
formatS a b = toStrict $ format a b

-- | Dealing with the underlying Proxy monad upon which Melvin clients are
-- based.
runMelvin :: s -> Effect (SafeT (StateT s IO)) r -> IO (Either SomeException r)
runMelvin st m = fmap Right (evalStateT (runSafeT $ runEffect m) st) `catch` (return . Left)
