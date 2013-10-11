{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Melvin.Damn.Tablumps (
  delump,
  Raw,
  linesOf
) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.RWS
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Default
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Melvin.Prelude hiding       (concatMap, cons, simple, takeWhile)
import qualified Prelude as P

data RenderState = RenderState
                 { _stDepth    :: Int
                 , _listDepth  :: Int
                 , _linkStack  :: [Text]
                 , _chromacity :: Bool
                 }

instance Default RenderState where
    def = RenderState 0 0 [] False

makeLenses ''RenderState

newtype Raw = Raw Text deriving NFData

linesOf :: Raw -> [Text]
linesOf (Raw m) = T.splitOn "\n" m

data Token = Lump Text [Text]
           | SimpleLump Text
           | NamedEntity Text
           | Entity Int
           | Plain Text
           deriving Show

tokenize :: Text -> Either String [Token]
tokenize = parseOnly (many1 tokenP <* endOfInput) where
    tokenP = foldr1 (<|>) [namedP, numP, plainP]
    namedP = do
        char '&'
        r <- takeWhile1 (liftA2 (||) isAlphaNum (=='/'))
        term <- char ';' <|> char '\t'
        case term of
            ';' -> return $ NamedEntity r
            '\t' -> getLump r
            _ -> $err' "unreachable"
    getLump n
        | n == "thumb"                        = lumpWithArgs n 6
        | n == "emote"                        = lumpWithArgs n 5
        | n == "link"                         = linkLump
        | n `elem` ["img", "iframe", "embed"] = lumpWithArgs n 3
        | n `elem` ["a", "dev", "avatar"]     = lumpWithArgs n 2
        | n `elem` ["abbr", "acro"]           = lumpWithArgs n 1
        | otherwise                           = return $ SimpleLump n
    lumpWithArgs name c = do
        args <- count c arg
        return $ Lump name args
    linkLump = do
        dest <- arg
        amp <- arg
        case amp of
            "&" -> return $ Lump "link" [dest]
            m -> arg *> return (Lump "link" [dest, m])
    arg = takeWhile (/= '\t') <* char '\t'
    numP = do
        string "&#"
        m <- peekChar
        r <- if m == Just 'x' || m == Just 'X'
                 then anyChar *> hexadecimal
                 else decimal
        char ';'
        return $ Entity r
    plainP = Plain <$> takeWhile1 (/= '&')

delump :: Text -> Raw
delump t = case tokenize t of
               Right ts -> Raw . T.strip $ foldState render def ts
               Left e -> $err' e

foldState :: Monoid m => (Token -> State RenderState m) -> RenderState -> [Token] -> m
foldState action sta (input:inputs) =
    let (text, newstate) = runState (action input) sta
     in text <> foldState action newstate inputs
foldState _a _s [] = mempty

render :: Token -> State RenderState Text
render (Plain t) = do
    sta <- gets (view stDepth)
    return $ if sta > 0 then strike t else t

render (SimpleLump n)
    | n `elem` ["/b", "/i", "/u"] = return "\15"
    | n `elem` ["br", "/p"] = return "\n"
    | n `elem` [ "sup", "/sup", "sub", "/sub"
               , "/iframe", "/embed", "p"
               , "code", "/code", "bcode", "/bcode"
               ] = return ""
render (SimpleLump "b") = return "\2"
render (SimpleLump "i") = return "\22"
render (SimpleLump "u") = return "\31"

render (SimpleLump "s") = stDepth += 1 >> return ""
render (SimpleLump "/s") = stDepth -= 1 >> return ""

render (Lump "a" (d:_)) = linkStack %= (d:) >> return ""
render (SimpleLump "/a") = do
    (m:_) <- linkStack <<%= tail
    return $ T.concat [" <", m, ">"]

render (Lump "dev" sy) = return $ T.concat sy

render (Lump "link" [d]) = return d
render (Lump "link" [d,title]) = return $ T.concat [d, " (", title, ")"]

render (Lump "emote" (s:_)) = return s

render (Lump "abbr" [t])
    | isChromacity t = chromacity .= True >> return ""
    | otherwise = return $ T.concat ["<abbr title='", t, "'>"]
render (SimpleLump "/abbr") = do
    c <- chromacity <<.= False
    return $ if c then "" else "</abbr>"

render (Lump "icon" (t:_)) = return $ T.concat [":icon", t, ":"]
render (Lump "embed" (t:_)) = return $ T.concat ["<embed src='", t, "' />"]
render (Lump "img" (t:_)) = return $ T.concat ["<img src='", t, "' />"]
render (Lump "iframe" (src:_)) = return $ T.concat ["<iframe src='", src, "' />"]
render (Lump "thumb" (_:t:_)) = return $ T.concat ["[thumb: ", t, "]"]

render (SimpleLump "ul") = listDepth += 1 >> return ""
render (SimpleLump "ol") = listDepth += 1 >> return ""
render (SimpleLump "/ul") = listDepth -= 1 >> return ""
render (SimpleLump "/ol") = listDepth -= 1 >> return ""

render (SimpleLump "li") = do
    r <- use listDepth
    let bull = case r of 1 -> "• " :: Text
                         2 -> "◦ "
                         _ -> "■ "
        pref = T.pack (replicate ((r - 1) * 2) ' ')
    return $ pref <> bull
render (SimpleLump "/li") = return "\n"

render (NamedEntity s) = return $ namedEntities M.! s

render (Entity n) = return $ T.singleton (chr n)

render l@(Lump{..}) = $err' $ "unknown tablump " ++ P.show l
render (SimpleLump s) = $err' $ "unknown simple tablump " ++ T.unpack s

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

namedEntities :: Map Text Text
namedEntities =
     M.fromList [ ("quot", "\34"), ("amp", "\38"), ("apos", "\39"), ("lt", "\60")
                , ("gt", "\62"), ("nbsp", "\160"), ("iexcl", "\161"), ("cent", "\162")
                , ("pound", "\163"), ("curren", "\164"), ("yen", "\165")
                , ("brvbar", "\166"), ("sect", "\167"), ("uml", "\168"), ("copy", "\169")
                , ("ordf", "\170"), ("laquo", "\171"), ("not", "\172"), ("shy", "\173")
                , ("reg", "\174"), ("macr", "\175"), ("deg", "\176"), ("plusmn", "\177")
                , ("sup2", "\178"), ("sup3", "\179"), ("acute", "\180"), ("micro", "\181")
                , ("para", "\182"), ("middot", "\183"), ("cedil", "\184"), ("sup1", "\185")
                , ("ordm", "\186"), ("raquo", "\187"), ("frac14", "\188")
                , ("frac12", "\189"), ("frac34", "\190"), ("iquest", "\191")
                , ("Agrave", "\192"), ("Aacute", "\193"), ("Acirc", "\194")
                , ("Atilde", "\195"), ("Auml", "\196"), ("Aring", "\197")
                , ("AElig", "\198"), ("Ccedil", "\199"), ("Egrave", "\200")
                , ("Eacute", "\201"), ("Ecirc", "\202"), ("Euml", "\203")
                , ("Igrave", "\204"), ("Iacute", "\205"), ("Icirc", "\206")
                , ("Iuml", "\207"), ("ETH", "\208"), ("Ntilde", "\209"), ("Ograve", "\210")
                , ("Oacute", "\211"), ("Ocirc", "\212"), ("Otilde", "\213")
                , ("Ouml", "\214"), ("times", "\215"), ("Oslash", "\216")
                , ("Ugrave", "\217"), ("Uacute", "\218"), ("Ucirc", "\219")
                , ("Uuml", "\220"), ("Yacute", "\221"), ("THORN", "\222")
                , ("szlig", "\223"), ("agrave", "\224"), ("aacute", "\225")
                , ("acirc", "\226"), ("atilde", "\227"), ("auml", "\228")
                , ("aring", "\229"), ("aelig", "\230"), ("ccedil", "\231")
                , ("egrave", "\232"), ("eacute", "\233"), ("ecirc", "\234")
                , ("euml", "\235"), ("igrave", "\236"), ("iacute", "\237")
                , ("icirc", "\238"), ("iuml", "\239"), ("eth", "\240"), ("ntilde", "\241")
                , ("ograve", "\242"), ("oacute", "\243"), ("ocirc", "\244")
                , ("otilde", "\245"), ("ouml", "\246"), ("divide", "\247")
                , ("oslash", "\248"), ("ugrave", "\249"), ("uacute", "\250")
                , ("ucirc", "\251"), ("uuml", "\252"), ("yacute", "\253")
                , ("thorn", "\254"), ("yuml", "\255"), ("OElig", "\338"), ("oelig", "\339")
                , ("Scaron", "\352"), ("scaron", "\353"), ("Yuml", "\376")
                , ("fnof", "\402"), ("circ", "\710"), ("tilde", "\732"), ("Alpha", "\913")
                , ("Beta", "\914"), ("Gamma", "\915"), ("Delta", "\916")
                , ("Epsilon", "\917"), ("Zeta", "\918"), ("Eta", "\919"), ("Theta", "\920")
                , ("Iota", "\921"), ("Kappa", "\922"), ("Lambda", "\923"), ("Mu", "\924")
                , ("Nu", "\925"), ("Xi", "\926"), ("Omicron", "\927"), ("Pi", "\928")
                , ("Rho", "\929"), ("Sigma", "\931"), ("Tau", "\932"), ("Upsilon", "\933")
                , ("Phi", "\934"), ("Chi", "\935"), ("Psi", "\936"), ("Omega", "\937")
                , ("alpha", "\945"), ("beta", "\946"), ("gamma", "\947"), ("delta", "\948")
                , ("epsilon", "\949"), ("zeta", "\950"), ("eta", "\951"), ("theta", "\952")
                , ("iota", "\953"), ("kappa", "\954"), ("lambda", "\955"), ("mu", "\956")
                , ("nu", "\957"), ("xi", "\958"), ("omicron", "\959"), ("pi", "\960")
                , ("rho", "\961"), ("sigmaf", "\962"), ("sigma", "\963"), ("tau", "\964")
                , ("upsilon", "\965"), ("phi", "\966"), ("chi", "\967"), ("psi", "\968")
                , ("omega", "\969"), ("thetasym", "\977"), ("upsih", "\978")
                , ("piv", "\982"), ("ensp", "\8194"), ("emsp", "\8195")
                , ("thinsp", "\8201"), ("zwnj", "\8204"), ("zwj", "\8205")
                , ("lrm", "\8206"), ("rlm", "\8207"), ("ndash", "\8211")
                , ("mdash", "\8212"), ("lsquo", "\8216"), ("rsquo", "\8217")
                , ("sbquo", "\8218"), ("ldquo", "\8220"), ("rdquo", "\8221")
                , ("bdquo", "\8222"), ("dagger", "\8224"), ("Dagger", "\8225")
                , ("bull", "\8226"), ("hellip", "\8230"), ("permil", "\8240")
                , ("prime", "\8242"), ("Prime", "\8243"), ("lsaquo", "\8249")
                , ("rsaquo", "\8250"), ("oline", "\8254"), ("frasl", "\8260")
                , ("euro", "\8364"), ("image", "\8465"), ("weierp", "\8472")
                , ("real", "\8476"), ("trade", "\8482"), ("alefsym", "\8501")
                , ("larr", "\8592"), ("uarr", "\8593"), ("rarr", "\8594")
                , ("darr", "\8595"), ("harr", "\8596"), ("crarr", "\8629")
                , ("lArr", "\8656"), ("uArr", "\8657"), ("rArr", "\8658")
                , ("dArr", "\8659"), ("hArr", "\8660"), ("forall", "\8704")
                , ("part", "\8706"), ("exist", "\8707"), ("empty", "\8709")
                , ("nabla", "\8711"), ("isin", "\8712"), ("notin", "\8713")
                , ("ni", "\8715"), ("prod", "\8719"), ("sum", "\8721"), ("minus", "\8722")
                , ("lowast", "\8727"), ("radic", "\8730"), ("prop", "\8733")
                , ("infin", "\8734"), ("ang", "\8736"), ("and", "\8743"), ("or", "\8744")
                , ("cap", "\8745"), ("cup", "\8746"), ("int", "\8747"), ("there4", "\8756")
                , ("sim", "\8764"), ("cong", "\8773"), ("asymp", "\8776"), ("ne", "\8800")
                , ("equiv", "\8801"), ("le", "\8804"), ("ge", "\8805"), ("sub", "\8834")
                , ("sup", "\8835"), ("nsub", "\8836"), ("sube", "\8838"), ("supe", "\8839")
                , ("oplus", "\8853"), ("otimes", "\8855"), ("perp", "\8869")
                , ("sdot", "\8901"), ("lceil", "\8968"), ("rceil", "\8969")
                , ("lfloor", "\8970"), ("rfloor", "\8971"), ("lang", "\9001")
                , ("rang", "\9002"), ("loz", "\9674"), ("spades", "\9824")
                , ("clubs", "\9827"), ("hearts", "\9829"), ("diams", "\9830")]
