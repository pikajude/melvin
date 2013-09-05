module Melvin.Damn.Actions (
  login,
  join,
  disconnect
) where

import Control.Proxy.Safe
import Data.Map                (fromList)
import Melvin.Chatrooms
import Melvin.Prelude
import Melvin.Types hiding     (token)
import Text.Damn.Packet hiding (render)

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

disconnect :: Packet
disconnect = Packet "disconnect" Nothing mempty Nothing
