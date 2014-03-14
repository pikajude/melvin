{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Melvin.Internal.Orphans where

import Control.Monad.Logger
import Control.Monad.State
import Pipes.Safe
import Prelude

instance MonadLogger m => MonadLogger (SafeT m) where
    monadLoggerLog a b c d = lift (monadLoggerLog a b c d)

instance MonadState s m => MonadState s (SafeT m) where
    get = lift get
    put = lift . put

instance MonadCatch m => MonadCatch (LoggingT m) where
    throwM = lift . throwM
    catch r h = LoggingT $ \i -> runLoggingT r i `catch` \e -> runLoggingT (h e) i
    mask a = LoggingT $ \i -> mask $ \u -> runLoggingT (a $ q u) i where
        q u (LoggingT m) = LoggingT (u . m)
    uninterruptibleMask a = LoggingT $ \i -> uninterruptibleMask $ \u -> runLoggingT (a $ q u) i where
        q u (LoggingT m) = LoggingT (u . m)
