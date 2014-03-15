{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import Data.List.Split
import Data.Machine
import Melvin.Prelude
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Instances ()

qc :: String -> IO ()
qc delim = do
    r <- quickCheckResult
            (\y -> run (source y ~> splittingBy (B.pack delim))
                == map B.pack (endBy delim (B.unpack $ B.concat y)))
    case r of
        Success{} -> return ()
        _ -> exitFailure

main :: IO ()
main = mapM_ qc ["\0", "\0\1", "\1\1"]
