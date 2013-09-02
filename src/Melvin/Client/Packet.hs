module Melvin.Client.Packet (
  Packet(..),
  parse,

  rplMyInfo,
  rplNotify,

  errNoNicknameGiven,
  errNeedMoreParams,
  errPasswordMismatch
) where

import           Control.Applicative         ((*>), (<*), (<|>))
import           Control.Monad
import           Data.Attoparsec.Text hiding (parse)
import           Data.Char
import qualified Data.Text as T
import           Melvin.Prelude hiding       (takeWhile)
import qualified Prelude as P

data Packet = Packet
        { pktPrefix :: Maybe Text
        , pktCommand :: Text
        , pktArguments :: [Text]
        } deriving Show

parse :: Text -> Packet
parse text = case parseOnly parser text of
    Left err -> error $ "Parsing failed (" ++ P.show err ++ ") for packet: " ++ P.show text
    Right pk -> pk

parser :: Parser Packet
parser = do
    prefix <- option Nothing prefixP
    command <- T.toUpper <$> commandP
    spaces
    params <- filter (not . T.null) <$> paramsP
    _ <- option "" crlf
    return $ Packet prefix command params
    where
        crlf = string "\r\n"
        badChars = "\x20\x00\x0d\x0a"
        spaces = void $ many1 space
        prefixP = Just <$> (char ':' *> hostP <* spaces)
        hostP = userP <|> serverP
        userP = takeWhile1 (notInClass badChars)
        serverP = takeWhile1 (inClass "a-z0-9.-")
        paramsP = (colonP <|> noColonP) `sepBy` spaces
        colonP = char ':' *> takeWhile (notInClass "\x00\x0d\x0a")
        noColonP = takeWhile (notInClass badChars)
        commandP = takeWhile1 isAlpha
               <|> (T.pack <$> count 3 digit)

render :: Packet -> Text
render (Packet pr c args) = maybe "" ((++" ") . T.cons ':') pr
                         ++ c
                         ++ (if null args then "" else " ")
                         ++ T.unwords (showArgs args)
                         ++ "\r\n"
    where showArgs [a] | " " `T.isInfixOf` a || T.null a || T.head a == ':'
                             = [':' `T.cons` a]
                       | otherwise = [a]
          showArgs (a:as) = a:showArgs as
          showArgs []     = mempty


-- | Predefined formatted packets
rplMyInfo :: Text -> Text
rplMyInfo n = render $ Packet hostname "004" [n, "chat.deviantart.com", "dAmnServer0.3", "qov", "i"]

rplNotify :: Text -> Text -> Text
rplNotify n msg = render $ Packet hostname "273" [n, msg]

errNoNicknameGiven, errNeedMoreParams, errPasswordMismatch :: Text -> Text
errNoNicknameGiven n = render $ Packet hostname "431" [n, "No nickname given"]
errNeedMoreParams n = render $ Packet hostname "461" [n, "Need more parameters"]
errPasswordMismatch n = render $ Packet hostname "464" [n, "Authentication failed. Try again."]

hostname :: Maybe Text
hostname = Just "chat.deviantart.com"
