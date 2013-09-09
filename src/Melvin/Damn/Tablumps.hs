{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Melvin.Damn.Tablumps (
  delump,
  lumps
) where

import           Control.Applicative
import           Control.Exception hiding    (try)
import           Data.Attoparsec.Text hiding (I)
import           Data.Char
import qualified Data.Text as T
import           Melvin.Exception
import           Melvin.Prelude hiding       (concatMap, cons, simple)

data Message = B [Message] | I [Message] | S [Message] | U [Message]
             | AnyChar Char
             deriving Show

delump :: Text -> Text
delump t = render t $ parseOnly lumps t

lumps :: Parser [Message]
lumps = many1 lump
    where
        lump = lumpB <|> lumpI <|> lumpS <|> lumpU
           <|> lumpAny
        lumpB = fmap B $ string "&b\t" *> lazy lump (string "&/b\t")
        lumpI = fmap I $ string "&i\t" *> lazy lump (string "&/i\t")
        lumpS = fmap S $ string "&s\t" *> lazy lump (string "&/s\t")
        lumpU = fmap U $ string "&u\t" *> lazy lump (string "&/u\t")
        lumpAny = AnyChar <$> anyChar

lazy :: Alternative f => f a -> f b -> f [a]
lazy a b = ([] <$ b) <|> ((:) <$> a <*> lazy a b)

render :: Text -> Either String [Message] -> Text
render t (Left s) = throw $ BadTablumps s t
render _ (Right msgs) = render' msgs

render' :: [Message] -> Text
render' (B ms:ns) = "\2" ++ render' ms ++ "\15" ++ render' ns
render' (I ms:ns) = "\22" ++ render' ms ++ "\15" ++ render' ns
render' (S ms:ns) = strike (render' ms) ++ render' ns
render' (U ms:ns) = "\31" ++ render' ms ++ "\15" ++ render' ns
render' (AnyChar c:ns) = T.singleton c ++ render' ns
render' [] = T.empty

strike :: Text -> Text
strike text = T.concatMap ?? text $ \ch ->
    if ord ch < ord ' '
        then T.singleton ch
        else T.cons ch (T.singleton '\822')
