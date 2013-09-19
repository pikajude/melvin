import Criterion.Main
import Data.Monoid
import Data.Text as T
import Melvin.Damn.Tablumps
import Prelude

main :: IO ()
main = defaultMain [
         bgroup "test" [
             bench "100" $ nf (unRaw . delump) ("&b\t" <> T.replicate 100 "&/b" <> "&/b\t")
           , bench "300" $ nf (unRaw . delump) ("&b\t" <> T.replicate 300 "&/b" <> "&/b\t")
           , bench "500" $ nf (unRaw . delump) ("&b\t" <> T.replicate 500 "&/b" <> "&/b\t")
           , bench "700" $ nf (unRaw . delump) ("&b\t" <> T.replicate 700 "&/b" <> "&/b\t")
         ]
       ]
