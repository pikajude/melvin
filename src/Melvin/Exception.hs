{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Melvin.Exception (
  MelvinException(..),
  isRetryable
) where

import Control.Exception
import Data.Text            (unpack)
import Data.Typeable
import Melvin.Prelude
import Prelude              (Show(..))

data MelvinException =
        -- | Client sent something that doesn't parse.
        ClientNoParse Text
        -- | Server sent something that doesn't parse.
      | ServerNoParse String Text
        -- | Client disconnected unexpectedly.
      | ClientSocketErr IOException
        -- | Server disconnected unexpectedly.
      | ServerSocketErr IOException
        -- | Server sent an unexpected "disconnect" packet.
      | ServerDisconnect Text
        -- | Authentication with dAmn failed, despite the fact that dAmn
        -- returned this authtoken. Weird.
      | AuthenticationFailed Text
        -- | We got an invalid tablump, which probably means the parser is
        -- wrong.
      | BadTablumps String Text
      deriving Typeable

instance Exception MelvinException

instance Show MelvinException where
    show (AuthenticationFailed e) = "couldn't login: " ++ unpack e
    show (ClientSocketErr e) = "lost connection to client: " ++ Prelude.show e
    show (ServerSocketErr e) = "lost connection to server: " ++ Prelude.show e
    show (ServerDisconnect e) = "lost connection to server: " ++ unpack e
    show ServerNoParse{..} = "received a bad packet from the server"
    show ClientNoParse{..} = "received a bad packet from the client"
    show (BadTablumps e _) = "failed parsing tablumps: " ++ e

isRetryable :: SomeException -> Bool
-- isRetryable e | Just (ServerDisconnect _) <- fromException e = True
isRetryable _ = False
