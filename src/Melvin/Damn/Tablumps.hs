module Melvin.Damn.Tablumps (
  delump
) where

import Data.Text
import Prelude   (($!))

simple :: [Text -> Text]
simple = [ replace "&br\t" "\n" ]

applyAll :: [b -> b] -> b -> b
applyAll (f:fs) a = applyAll fs $! (f a)
applyAll [] a = a

delump :: Text -> Text
delump text = applyAll simple text
