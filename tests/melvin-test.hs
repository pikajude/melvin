{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger
import Melvin
import Melvin.Options
import Network

main :: IO ()
main = doAMelvin (Mopts (PortNumber 6667) LevelDebug Nothing) []
