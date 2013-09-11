module Melvin.Damn.Actions (
  pong,
  login,
  join,
  part,
  msg,
  action,
  disconnect
) where

import Data.Map                (fromList)
import Melvin.Chatrooms
import Melvin.Damn.HTML
import Melvin.Prelude hiding   (re)
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

join :: Chatroom -> ClientT Packet
join r = do
    room <- render r
    return $ Packet "join" (Just room) mempty Nothing

part :: Chatroom -> ClientT Packet
part r = do
    room <- render r
    return $ Packet "part" (Just room) mempty Nothing

msg :: Chatroom -> Text -> ClientT Packet
msg c m = do
    room <- render c
    let subpkt = Packet "msg" (Just "main") mempty (Just $ escape m)
        parent = Packet "send" (Just room) mempty Nothing & pktSubpacketL ?~ subpkt
    return parent

action :: Chatroom -> Text -> ClientT Packet
action c m = do
    room <- render c
    let subpkt = Packet "action" (Just "main") mempty (Just $ escape m)
        parent = Packet "send" (Just room) mempty Nothing & pktSubpacketL ?~ subpkt
    return parent

disconnect :: Packet
disconnect = Packet "disconnect" Nothing mempty Nothing
