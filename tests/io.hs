{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import Data.List.Split
import Data.Machine
import Melvin.Prelude
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Instances ()

qc f = do
    r <- quickCheckResult f
    case r of
        Success{} -> return ()
        _ -> exitFailure

main :: IO ()
main = do
    qc (\y -> run (source y ~> splittingBy "\0")
        == map B.pack (endBy "\0" (B.unpack $ B.concat y)))
    qc (\y -> run (source y ~> splittingBy "\0\1")
        == map B.pack (endBy "\0\1" (B.unpack $ B.concat y)))
    qc (\y -> run (source y ~> splittingBy "\1\0")
        == map B.pack (endBy "\1\0" (B.unpack $ B.concat y)))
