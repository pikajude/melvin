module Melvin.Chatrooms (
  fromChannel,
  toChatroom,
  toChannel,
  fromChatroom,
  render
) where

import           Control.Monad
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

render :: ClientT m => Chatroom -> m Text
render (Chatroom s) = return $ "chat:" ++ s
render (PrivateChat u) = do
    uname <- use username
    let pair = sortBy (comparing T.toLower) [uname, u]
    return $ "pchat:" ++ T.intercalate ":" pair

toChannel :: ClientT m => Text -> m Text
toChannel text
    | "chat:" `T.isPrefixOf` text = return $ '#' `T.cons` T.drop 5 text
    | otherwise = do
        me <- use username
        let usernames = T.splitOn ":" (T.drop 6 text)
        return . T.cons '&' . $fromJst $ find (/= me) usernames

fromChatroom :: ClientT m => Text -> m Text
fromChatroom = toChannel
