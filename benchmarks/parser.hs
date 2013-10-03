import Criterion.Main
import Data.Monoid
import Data.Text as T
import Melvin.Damn.Tablumps
import Prelude

main :: IO ()
main = defaultMain [
         bgroup "test" [
             bench "100" $ nf delump ("&b\t" <> T.replicate 10 "&amp;" <> "&/b\t")
           , bench "300" $ nf delump ("&b\t" <> T.replicate 30 "&amp;" <> "&/b\t")
           , bench "500" $ nf delump ("&b\t" <> T.replicate 50 "&amp;" <> "&/b\t")
           , bench "700" $ nf delump ("&b\t" <> T.replicate 70 "&amp;" <> "&/b\t")
         ]
       ]
