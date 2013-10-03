{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Melvin.Prelude.TH (error) where

import Language.Haskell.TH
import Prelude hiding (error)
import qualified Prelude as P

error :: Q Exp
error = do
    loc <- location
    [e|(.) P.error ((++) (concat [$(stringE (loc_filename loc))
                               , ":"
                               , $(stringE . show . fst $ loc_start loc)
                               , ":"
                               , $(stringE . show . snd $ loc_start loc)
                               , ": "]))|]
