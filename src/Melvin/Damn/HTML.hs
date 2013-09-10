{-# LANGUAGE RecordWildCards #-}

module Melvin.Damn.HTML (
  escape, unescape
) where

import           Data.Char
import qualified Data.Text as T
import           Data.Text.Read
import           Melvin.Prelude

escape :: Text -> Text
escape = T.concatMap go
    where
        go '&' = "&amp;"
        go '"' = "&quot;"
        go '\'' = "&apos;"
        go x | ord x < 127 = T.singleton x
        go s = formatS "&#{};" [show $ ord s]

unescape :: Text -> Text
unescape t = case T.uncons t of
    Nothing -> mempty
    Just ('&', y) | "#x" `T.isPrefixOf` y -> case hexadecimal (T.drop 2 y) of
        Right (i,yy) | Just (';', yyy) <- T.uncons yy -> T.singleton (chr i) ++ unescape yyy
        _ -> T.singleton '&' ++ unescape y
    Just ('&', y) | "#" `T.isPrefixOf` y -> case decimal (T.drop 1 y) of
        Right (i,yy) | Just (';', yyy) <- T.uncons yy -> T.singleton (chr i) ++ unescape yyy
        _ -> T.singleton '&' ++ unescape y
    Just (x,y) -> T.singleton x ++ unescape y
