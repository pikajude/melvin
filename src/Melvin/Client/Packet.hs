{-# LANGUAGE TemplateHaskell #-}

module Melvin.Client.Packet (
  Packet(..),
  parse,
  render,

  cmdPong,
  cmdJoin,
  cmdDupJoin,
  cmdPart,
  cmdDupPart,
  cmdModeChange,
  cmdPrivmsg,
  cmdPrivaction,
  cmdSendError,
  cmdKick,

  cmdPcCreate,
  cmdPcUpdate,
  cmdPcMove,
  cmdModeUpdate,

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
import           Data.Maybe
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
    Left e -> $err' $ "Parsing failed (" ++ P.show e ++ ") for packet: " ++ P.show text
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
cmdPong :: [Text] -> Packet
cmdPong = Packet Nothing "PONG"

cmdJoin, cmdSendError :: Text -> Text -> Packet
cmdJoin n channel = Packet (hostOf n) "JOIN" [channel]
cmdSendError channel e = Packet (Just "dAmn") "NOTICE"
    [channel, [st|Send error: %s|] e]

cmdPrivmsg, cmdPrivaction :: Text -> Text -> Text -> Packet
cmdPrivmsg n channel text = Packet (hostOf n) "PRIVMSG"
    [channel, T.cons ':' $ bullets text]
cmdPrivaction n channel text = Packet (hostOf n) "PRIVMSG"
    [channel, ac . bullets $ text]
    where ac m = "\1ACTION " ++ m ++ "\1"

cmdPart, cmdModeChange :: Text -> Text -> Text -> Packet
cmdPart n channel reason = Packet (hostOf n) "PART" [channel, reason]
cmdModeChange channel u m = cmdModeUpdate channel u Nothing (Just m)

cmdPcMove, cmdPcCreate, cmdPcUpdate :: Text -> Text -> Text -> Text -> Packet
cmdPcMove channel u pc mover = Packet (Just "dAmn") "NOTICE"
    [channel, [st|%s has been moved to %s by %s|] u pc mover]

cmdPcCreate channel pcname privs creator = Packet (Just "dAmn") "NOTICE"
    [channel, [st|%s has created group %s with privileges: %s|] creator pcname privs]

cmdPcUpdate channel pcname privs creator = Packet (Just "dAmn") "NOTICE"
    [channel, [st|%s has updated group %s with privileges: %s|] creator pcname privs]

cmdModeUpdate :: Text -> Text -> Maybe Text -> Maybe Text -> Packet
cmdModeUpdate channel u old new = Packet (Just "dAmn") "MODE"
    ([channel] ++ modestr ++ userargs)
    where (modestr,userargs) = case (old,new) of
              (Nothing,Nothing) -> ([],[])
              (Nothing,Just m) -> (["+" ++ m], [u])
              (Just m,Nothing) -> (["-" ++ m], [u])
              (Just m,Just m') -> (["-" ++ m ++ "+" ++ m'], [u, u])

cmdKick :: Text -> Text -> Text -> Maybe Text -> Packet
cmdKick kicker channel receiver reason =
    Packet (hostOf kicker) "KICK" [channel, receiver, fromMaybe "no reason" reason]

cmdDupJoin, cmdDupPart :: Text -> Text -> Int -> Packet
cmdDupJoin n channel cnt = Packet (Just "dAmn") "NOTICE"
    [channel, [st|%s has joined again (now joined %s)|] n (readable cnt)]
cmdDupPart n channel cnt = Packet (Just "dAmn") "NOTICE"
    [channel, [st|%s has left (now joined %s)|] n (readable cnt)]

readable :: (Eq a, Num a, Show a) => a -> Text
readable 1 = "once"
readable 2 = "twice"
readable m = show m ++ " times"

-- | Reply packets
rplMyInfo :: Text -> Packet
rplMyInfo n = Packet hostname "004" [n, "chat.deviantart.com", "dAmnServer0.3", "qov", "i"]

rplNotify :: Text -> Text -> Packet
rplNotify n msg = Packet hostname "273" [n, msg]

rplNoTopic, rplTopic :: Text -> Text -> Text -> Packet
rplNoTopic user channel reason = Packet hostname "331" [user, channel, reason]
rplTopic user channel topic = Packet hostname "332" [user, channel, bullets topic]

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

hostOf :: Text -> Maybe Text
hostOf = Just . join [st|%s!%s@chat.deviantart.com|]

bullets :: Text -> Text
bullets t = T.replace ":bulletred:" "\ETX5●\SI"
          . T.replace ":bulletorange:" "\ETX4●\SI"
          . T.replace ":bulletyellow:" "\ETX7●\SI"
          . T.replace ":bulletgreen:" "\ETX3●\SI"
          . T.replace ":bulletblue:" "\ETX2●\SI"
          . T.replace ":bulletpurple:" "\ETX13●\SI"
          . T.replace ":bulletpink:" "\ETX6●\SI"
          . T.replace ":bulletwhite:" "\ETX0●\SI"
          . T.replace ":bulletblack:" "\ETX1●\SI"
          $ t

{-# ANN module ("HLint: ignore Use :" :: String) #-}
