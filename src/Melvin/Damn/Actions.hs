module Melvin.Damn.Actions (
  join
) where

import Control.Proxy.Safe
import Melvin.Chatrooms
import Melvin.Prelude
import Melvin.Types

join :: Chatroom -> ClientP a' a b' b SafeIO ()
join r = do
    room <- render r
    writeServer $ formatS "join {}\n" [room]
