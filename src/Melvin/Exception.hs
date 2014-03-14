{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Melvin.Exception (
  MelvinException(..),
  isRetryable
) where

import Control.Exception
import Data.ByteString      (ByteString)
import Data.Monoid          ((<>))
import Data.Text            (unpack, Text)
import Data.Typeable
import Prelude hiding       ((++))
import System.IO.Error

data MelvinException =
        -- | Client sent something that doesn't parse.
        ClientNoParse ByteString
        -- | Server sent something that doesn't parse.
      | ServerNoParse String ByteString
        -- | Client disconnected unexpectedly.
      | ClientSocketErr IOException
        -- | Server disconnected unexpectedly.
      | ServerSocketErr IOException
        -- | Server never actually got connected, but someone is trying to
        -- write to it.
      | ServerNotConnected
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
    show (AuthenticationFailed e) = "couldn't login: " <> unpack e
    show (ClientSocketErr e) = "lost connection to client: " <> Prelude.show e
    show (ServerSocketErr e) = "lost connection to server: " <> Prelude.show e
    show ServerNotConnected = "tried to write to server, but handle not connected"
    show (ServerDisconnect e) = "lost connection to server: " <> unpack e
    show ServerNoParse{..} = "received a bad packet from the server"
    show ClientNoParse{..} = "received a bad packet from the client"
    show (BadTablumps e _) = "failed parsing tablumps: " <> e

isRetryable :: SomeException -> Bool
isRetryable e | Just (ServerDisconnect _) <- fromException e = True
isRetryable e = maybe False (not . isDoesNotExistError) $ cast e
