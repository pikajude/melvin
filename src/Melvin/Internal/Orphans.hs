{-# OPTIONS_GHC -fno-warn-orphans -Werror #-}

module Melvin.Internal.Orphans where

import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.State
import Data.Machine
import Prelude

instance MonadCatch m => MonadCatch (LoggingT m) where
    throwM = lift . throwM
    catch r h = LoggingT $ \i -> runLoggingT r i `catch` \e -> runLoggingT (h e) i
    mask a = LoggingT $ \i -> mask $ \u -> runLoggingT (a $ q u) i where
        q u (LoggingT m) = LoggingT (u . m)
    uninterruptibleMask a = LoggingT $ \i -> uninterruptibleMask $ \u -> runLoggingT (a $ q u) i where
        q u (LoggingT m) = LoggingT (u . m)

instance MonadCatch m => MonadCatch (NoLoggingT m) where
    throwM = lift . throwM
    catch r h = NoLoggingT $ runNoLoggingT r `catch` \e -> runNoLoggingT (h e)
    mask a = NoLoggingT $ mask $ \u -> runNoLoggingT (a $ q u) where
        q u (NoLoggingT m) = NoLoggingT (u m)
    uninterruptibleMask a = NoLoggingT $ uninterruptibleMask $ \u -> runNoLoggingT (a $ q u) where
        q u (NoLoggingT m) = NoLoggingT (u m)

instance MonadLogger m => MonadLogger (PlanT k b m) where
    monadLoggerLog a b c d = lift (monadLoggerLog a b c d)
