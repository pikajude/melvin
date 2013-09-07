module Melvin.Chatrooms (
  fromChannel,
  toChatroom,
  toChannel,
  fromChatroom,
  render
) where

import           Control.Monad
import           Control.Proxy.Safe
import           Data.Char
import           Data.Function
import           Data.List hiding ((++))
import           Data.Maybe
import           Data.Ord
import qualified Data.Text as T
import           Melvin.Prelude
import           Melvin.Types

toChatroom :: Text -> Maybe Chatroom
toChatroom text = case T.uncons text of
        Just ('#', r) | isValidRoomName r -> Just $ Chatroom r
        Just ('&', r) | isValidRoomName r -> Just $ PrivateChat r
        _ -> Nothing
    where isValidRoomName = T.all $ \x ->
            any ($ x) [ isAsciiUpper, isAsciiLower
                      , isDigit, (=='_'), (=='-') ]

fromChannel :: Text -> Maybe Chatroom
fromChannel = toChatroom

render :: Chatroom -> ClientP a' a b' b SafeIO Text
render (Chatroom s) = return $ "chat:" ++ s
render (PrivateChat u) = do
    uname <- liftP $ gets (view username)
    let pair = sortBy (comparing T.toLower) [uname, u]
    return $ "pchat:" ++ T.intercalate ":" pair

toChannel :: Text -> ClientP a' a b' b SafeIO Text
toChannel text
    | "chat:" `T.isPrefixOf` text = return $ '#' `T.cons` T.drop 5 text
    | otherwise = do
        me <- liftP $ gets (view username)
        let usernames = T.splitOn ":" (T.drop 6 text)
        return . T.cons '&' . fromJust $ find (/= me) usernames

fromChatroom :: Text -> ClientP a' a b' b SafeIO Text
fromChatroom = toChannel
