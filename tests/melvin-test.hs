{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.Async
import Control.Monad.Logger
import Melvin
import Melvin.Options
import Network
import Prelude
import StubDamn

main :: IO ()
main = do
    concurrently
        (doAMelvin (Mopts (PortNumber 6667) LevelDebug Nothing) [])
        runStubDamn
    return ()
