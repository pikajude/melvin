{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Melvin.Damn.Tablumps (
  delump,
  lump
) where

import           Control.Applicative
import           Control.Exception hiding    (try)
import           Data.Attoparsec.Text hiding (I)
import           Data.Char
import qualified Data.Text as T
import           Melvin.Exception
import           Melvin.Prelude hiding       (concatMap, cons, simple, takeWhile)

data Message = S [Message] | A Text Text [Message] | Dev Char Text
             | Chunk Char
             deriving Show

delump :: Text -> Text
delump t = render t . parseOnly (many1 lump) $ simple t

simple :: Text -> Text
simple = foldr ($!) ?? [
               T.replace "&br\t" "\n"
             , T.replace "&b\t" "\2", T.replace "&/b\t" "\15"
             , T.replace "&i\t" "\22", T.replace "&/i\t" "\15"
             , T.replace "&u\t" "\31", T.replace "&/u\t" "\15"
             , T.replace "&sup\t" "", T.replace "&/sup\t" ""
             , T.replace "&sub\t" "", T.replace "&/sub\t" ""
             ]

lump :: Parser Message
lump = foldr1 (<|>) [ lumpS, lumpA, lumpDev, Chunk <$> anyChar ]
    where
        lumpS = fmap S $ string "&s\t" *> lazy lump (string "&/s\t")
        arg = takeWhile (/= '\t') <* char '\t'
        lumpA = do
            string "&a\t"
            dest <- arg
            title <- arg
            contents <- lazy lump (string "&/a\t")
            return $ A dest title contents
        lumpDev = do
            string "&dev\t"
            s <- anyChar
            char '\t'
            Dev s <$> arg

lazy :: Alternative f => f a -> f b -> f [a]
lazy a b = ([] <$ b) <|> ((:) <$> a <*> lazy a b)

render :: Text -> Either String [Message] -> Text
render t (Left s) = throw $ BadTablumps s t
render _ (Right msgs) = render' msgs

render' :: [Message] -> Text
render' (S ms:ns) = strike (render' ms) ++ render' ns
render' (A dest _ contents:ns) = T.concat [render' contents, " <", dest, ">"] ++ render' ns
render' (Dev c n:ns) = T.cons c n ++ render' ns
render' (Chunk t:ns) = T.cons t $ render' ns
render' [] = T.empty

strike :: Text -> Text
strike text = T.concatMap ?? text $ \ch ->
    if ord ch < ord ' '
        then T.singleton ch
        else T.cons ch (T.singleton '\822')
