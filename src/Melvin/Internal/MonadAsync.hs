{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Melvin.Internal.MonadAsync (async, wait) where

import qualified Control.Concurrent.Async as A
import Control.Monad.Fix
import Control.Monad.Logger
import Control.Monad.State
import Prelude

class Monad m => MonadAsync m where
    async :: m a -> m (A.Async a)
    wait :: A.Async a -> m a

instance MonadAsync IO where
    async = A.async
    wait = A.wait

instance MonadAsync m => MonadAsync (LoggingT m) where
    async f = LoggingT $ \i -> async (runLoggingT f i)
    wait = lift . wait

instance MonadAsync m => MonadAsync (StateT s m) where
    async m = StateT $ \s -> liftM (flip (,) s) $ async (evalStateT m s)
    wait = lift . wait

instance MonadFix (LoggingT IO) where
    mfix f = LoggingT $ \i -> mfix $ \a -> runLoggingT (f a) i
