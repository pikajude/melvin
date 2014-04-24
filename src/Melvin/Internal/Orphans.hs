{-# OPTIONS_GHC -fno-warn-orphans #-}

module Melvin.Internal.Orphans where

import Control.Monad.Logger
import Control.Monad.State
import Data.Machine

instance MonadLogger m => MonadLogger (PlanT k b m) where
    monadLoggerLog a b c d = lift (monadLoggerLog a b c d)
