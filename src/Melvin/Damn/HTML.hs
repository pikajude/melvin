module Melvin.Damn.HTML (
  escape,
  message,
  Message(..)
) where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char
import qualified Data.Text as T
import           Melvin.Prelude hiding (takeWhile)

data Message = Numeric Int | Named Text | Chunk Text
             deriving Show

message :: Parser [Message]
message = many (numP <|> namedP <|> chunkP) <* endOfInput
    where
        numP = fmap Numeric $ (string "&#x" *> inRange hexadecimal <* char ';')
                          <|> (string "&#" *> inRange decimal <* char ';')
        namedP = fmap Named $ string "&" *> takeTill (== ';') <* char ';'
        chunkP = Chunk <$> takeWhile1 (/= '&')
        inRange a = do
            c <- a
            guard (c <= ord maxBound)
            return c

escape :: Text -> Text
escape = T.concatMap go
    where
        go '"' = "&quot;"
        go '\'' = "&apos;"
        go x | ord x < 127 = T.singleton x
        go s = formatS "&#{};" [show $ ord s]
