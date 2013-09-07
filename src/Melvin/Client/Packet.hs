module Melvin.Client.Packet (
  Packet(..),
  parse,
  render,

  cmdJoin,
  cmdPrivmsg,
  cmdPrivaction,
  cmdDupJoin,

  rplMyInfo,
  rplNotify,
  rplNoTopic,
  rplTopic,
  rplTopicWhoTime,
  rplNameReply,

  errNoNicknameGiven,
  errNeedMoreParams,
  errPasswordMismatch,
  errNoSuchChannel,
  errBannedFromChan
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
    where showArgs [a] | T.head a == ':' = [a]
                       | " " `T.isInfixOf` a || T.null a = [':' `T.cons` a]
                       | otherwise = [a]
          showArgs (a:as) = a:showArgs as
          showArgs []     = mempty


-- | Predefined formatted packets

-- | Packets that aren't reply packets
cmdJoin :: Text -> Text -> Packet
cmdJoin n channel = Packet host "JOIN" [channel]
    where host = Just $ formatS "{}!{}@chat.deviantart.com" [n, n]

cmdPrivmsg, cmdPrivaction :: Text -> Text -> Text -> Packet
cmdPrivmsg n channel text = Packet host "PRIVMSG" [channel, text]
    where host = Just $ formatS "{}!{}@chat.deviantart.com" [n, n]

cmdPrivaction n channel text = Packet host "PRIVMSG" [channel, ac text]
    where host = Just $ formatS "{}!{}@chat.deviantart.com" [n, n]
          ac m = "\1ACTION " ++ m ++ "\1"

cmdDupJoin :: Text -> Text -> Int -> Packet
cmdDupJoin n channel cnt = Packet hostname "NOTICE"
    [channel, formatS "{} has joined again ({} times)" [n, show cnt]]

-- | Reply packets
rplMyInfo :: Text -> Packet
rplMyInfo n = Packet hostname "004" [n, "chat.deviantart.com", "dAmnServer0.3", "qov", "i"]

rplNotify :: Text -> Text -> Packet
rplNotify n msg = Packet hostname "273" [n, msg]

rplNoTopic, rplTopic :: Text -> Text -> Text -> Packet
rplNoTopic user channel reason = Packet hostname "331" [user, channel, reason]
rplTopic user channel topic = Packet hostname "332" [user, channel, topic]

rplTopicWhoTime :: Text -> Text -> Text -> Text -> Packet
rplTopicWhoTime user channel setter time =
        Packet hostname "333" [user, channel, setter, time]

rplNameReply :: Text -> Text -> [Text] -> Packet
rplNameReply channel user users =
        Packet hostname "353" [user, "=", channel, T.intercalate " " users]

-- | Error response packets
errNoNicknameGiven, errNeedMoreParams, errPasswordMismatch :: Text -> Packet
errNoNicknameGiven  n = Packet hostname "431" [n, "No nickname given"]
errNeedMoreParams   n = Packet hostname "461" [n, "Need more parameters"]
errPasswordMismatch n = Packet hostname "464" [n, "Authentication failed. Try again."]

errNoSuchChannel, errBannedFromChan :: Text -> Text -> Packet
errNoSuchChannel  n t = Packet hostname "403" [n, t, "No such channel"]
errBannedFromChan n t = Packet hostname "474" [n, t, "Not privileged"]

hostname :: Maybe Text
hostname = Just "chat.deviantart.com"
