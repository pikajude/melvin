module Melvin.Damn.Actions (
  pong,
  login,
  join,
  msg,
  disconnect
) where

import Control.Proxy.Safe
import Data.Map                (fromList)
import Melvin.Chatrooms
import Melvin.Prelude
import Melvin.Types hiding     (token)
import Text.Damn.Packet hiding (render)

pong :: Packet
pong = Packet "pong" Nothing mempty Nothing

login :: Text -> Text -> Packet
login user token = Packet
    { pktCommand = "login"
    , pktParameter = Just user
    , pktArgs = fromList [("pk", token)]
    , pktBody = Nothing
    }

join :: Chatroom -> ClientP a' a b' b SafeIO Packet
join r = do
    room <- render r
    return $ Packet "join" (Just room) mempty Nothing

msg :: Chatroom -> Text -> ClientP a' a b' b SafeIO Packet
msg c m = do
    room <- render c
    let subpkt = Packet "msg" (Just "main") mempty (Just m)
        parent = Packet "send" (Just room) mempty Nothing & pktSubpacketL ?~ subpkt
    return parent

disconnect :: Packet
disconnect = Packet "disconnect" Nothing mempty Nothing
