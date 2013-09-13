{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Melvin.Damn.Tablumps (
  delump,
  Raw,
  unRaw,
  lines
) where

import           Control.Applicative
import           Control.Exception hiding    (try)
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Maybe
import qualified Data.Text as T
import           Melvin.Exception
import           Melvin.Prelude hiding       (concatMap, cons, simple, takeWhile)

newtype Raw = Raw { unRaw :: Text }

lines :: Raw -> [Raw]
lines m = map Raw . T.splitOn "\n" $ unRaw m

data Message = S [Message] | A Text Text [Message] | Dev Char Text
             | Code [Message] | Abbr Text [Message] | Link Text (Maybe Text)
             | Ul [Message] | Ol [Message] | Li Int [Message]
             | Icon Text Text
             | Embed Text Text Text | Iframe Text Text Text | Img Text Text Text
             | Emote Text Text Text Text Text
             | Thumb Text Text Text Text Text Text
             | Chunk Char
             deriving Show

delump :: Text -> Raw
delump t = Raw . T.strip . render t . parseOnly (many1 (lump 0)) $ simple t

simple :: Text -> Text
simple = foldr ($!) ?? [
               T.replace "&br\t" "\n"
             , T.replace "&b\t" "\2",  T.replace "&/b\t" "\15"
             , T.replace "&i\t" "\22", T.replace "&/i\t" "\15"
             , T.replace "&u\t" "\31", T.replace "&/u\t" "\15"
             , T.replace "&sup\t" "",  T.replace "&/sup\t" ""
             , T.replace "&sub\t" "",  T.replace "&/sub\t" ""
             , T.replace "&/iframe\t" "", T.replace "&/embed\t" ""
             , T.replace "&p\t" "", T.replace "&/p\t" "\n"
             , T.replace "&code\t" "", T.replace "&/code\t" ""
             , T.replace "&bcode\t" "", T.replace "&/bcode\t" ""
             ]

lump :: Int -> Parser Message
lump nn = foldr (\a b -> a nn <|> b) mzero
             [ lumpS, lumpA, lumpDev, lumpEmote, lumpCode, lumpLink
             , lumpUl, lumpOl, lumpLi
             , lumpAbbr, lumpThumb, lumpIcon, lumpEmbed, lumpImg, lumpIframe
             , \_ -> Chunk <$> anyChar]
    where
        arg = takeWhile (/= '\t') <* char '\t'
        lumpS n = fmap S $ string "&s\t" *> lazy (lump n) (string "&/s\t")
        lumpCode n = fmap Code $ string "&code\t" *> lazy (lump n) (string "&/code\t")
        lumpLink _ = do
            string "&link\t"
            dest <- arg
            m <- arg
            if m == "&"
                then return $ Link dest Nothing
                else Link dest (Just m) <$ arg
        lumpA n = do
            string "&a\t"
            dest <- arg
            title <- arg
            contents <- lazy (lump n) (string "&/a\t")
            return $ A dest title contents
        lumpUl n = fmap Ul $ string "&ul\t" *> lazy (lump $ n + 1) (string "&/ul\t")
        lumpOl n = fmap Ol $ string "&ol\t" *> lazy (lump $ n + 1) (string "&/ol\t")
        lumpLi n = fmap (Li n) $ string "&li\t" *> lazy (lump n) (string "&/li\t")
        lumpDev _ = do
            string "&dev\t"
            s <- anyChar
            char '\t'
            Dev s <$> arg
        lumpEmote _ = do
            string "&emote\t"
            Emote <$> arg <*> arg <*> arg <*> arg <*> arg
        lumpIcon _ = do
            string "&avatar\t"
            Icon <$> arg <*> arg
        lumpAbbr n = do
            string "&abbr\t"
            title <- arg
            contents <- lazy (lump n) (string "&/abbr\t")
            return $ Abbr title contents
        lumpEmbed _ = do
            string "&embed\t"
            Embed <$> arg <*> arg <*> arg
        lumpImg _ = do
            string "&img\t"
            Img <$> arg <*> arg <*> arg
        lumpIframe _ = do
            string "&iframe\t"
            Iframe <$> arg <*> arg <*> arg
        lumpThumb _ = do
            string "&thumb\t"
            Thumb <$> arg <*> arg <*> arg <*> arg <*> arg <*> arg

lazy :: Alternative f => f a -> f b -> f [a]
lazy a b = ([] <$ b) <|> ((:) <$> a <*> lazy a b)

render :: Text -> Either String [Message] -> Text
render t (Left s) = throw $ BadTablumps s t
render _ (Right msgs) = render' msgs

render' :: [Message] -> Text
render' (S ms:ns)              = strike (render' ms) ++ render' ns
render' (A dest _ contents:ns) = T.concat [render' contents, " <", dest, ">"] ++ render' ns
render' (Dev c n:ns)           = T.cons c n ++ render' ns
render' (Code ms:ns)           = render' ms ++ render' ns
render' (Link s Nothing:ns)    = s ++ render' ns
render' (Link s (Just t):ns)   = T.concat [s, " (", t, ")"] ++ render' ns
render' (Emote s _ _ _ _:ns)   = s ++ render' ns
render' (Abbr t ms:ns) | null ms && isChromacity t = render' ns
render' (Abbr t ms:ns)         = T.concat ["<abbr title='", t, "'>", render' ms, "</abbr>"] ++ render' ns
render' (Icon t _:ns)          = T.concat [":icon", t, ":"] ++ render' ns
render' (Embed t _ _:ns)       = T.concat ["<embed src='", t, "' />"] ++ render' ns
render' (Img t _ _:ns)         = T.concat ["<img src='", t, "' />"] ++ render' ns
render' (Iframe src _ _:ns)    = T.concat ["<iframe src='", src, "' />"] ++ render' ns
render' (Thumb _ t _ _ _ _:ns) = T.concat ["[thumb: ", t, "]"] ++ render' ns
render' (Ul ms:ns)             = "\n" ++ render' ms ++ render' ns
render' (Ol ms:ns)             = "\n" ++ render' ms ++ render' ns
render' (Li n ms:ns)           = T.pack (replicate ((n - 1) * 2) ' ')
                              ++ bull ++ render' ms ++ nl ++ render' ns
    where nl = case ns of
                   (Li _ _:_) -> "\n"
                   _ -> ""
          bull = case n of 1 -> "• " :: Text
                           2 -> "◦ "
                           _ -> "■ "
render' (Chunk t:ns)           = T.cons t $! render' ns
render' []                     = T.empty

isChromacity :: Text -> Bool
isChromacity t = length items == 3
              && head items == "colors"
              && T.all isHexDigit (items !! 1)
              && T.all isHexDigit (items !! 2)
    where items = T.splitOn ":" t

strike :: Text -> Text
strike text = T.concatMap ?? text $ \ch ->
    if ord ch < ord ' '
        then T.singleton ch
        else T.cons ch (T.singleton '\822')
