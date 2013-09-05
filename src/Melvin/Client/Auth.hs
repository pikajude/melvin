{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Melvin.Client.Auth (
    authenticate
) where

import           Control.Exception
import           Control.Monad.State
import           Data.Maybe
import qualified Data.Text as T
import           Melvin.Client.Packet
import           Melvin.Logger
import           Melvin.Prelude hiding (get, respond)
import           Melvin.Token

data AuthClient = AuthClient
        { _acNick     :: Maybe Text
        , _acUsername :: Maybe Text
        , _acPassword :: Maybe Text
        } deriving Show

makeLenses ''AuthClient

type Username = Text
type AuthState = StateT AuthClient IO

authHandler :: SomeException -> IO (Either SomeException (Username, Text))
authHandler = return . Left

authenticate :: Handle -> IO (Either SomeException (Username, Text))
authenticate h = handle authHandler $
    (`evalStateT` AuthClient Nothing Nothing Nothing) . fix $ \f -> do
        ai <- getAuthInfo h
        case ai of
            Nothing -> do
                authFailure h
                f
            Just t -> do
                authSuccess h
                return $ Right t

greet :: Handle -> Packet -> AuthState ()
greet h Packet {
      pktArguments = (nick:_)
    } = do
    acNick .= Just nick
    write h $ formatS ":chat.deviantart.com 001 {} :Welcome to dAmn {}!{}@chat.deviantart.com\r\n" [nick, nick, nick]
    write h $ formatS ":chat.deviantart.com 002 {} :Your host is chat.deviantart.com, running dAmnServer 0.3\r\n" [nick]
    write h $ formatS ":chat.deviantart.com 004 {} chat.deviantart.com dAmnServer0.3 qov i\r\n" [nick]
    write h $ formatS ":chat.deviantart.com 005 {} PREFIX=(qov)~@+\r\n" [nick]
greet h _ = write h . render $ errNoNicknameGiven "stupid"

respond :: Handle -> Text -> Packet -> AuthState ()
respond h text packet =
    case text of
        "NICK" -> case pktArguments packet of
                      [] -> write h . render $ errNoNicknameGiven "stupid"
                      (us:_) -> do
                          acUsername ?= us
                          greet h packet
        "PASS" -> case pktArguments packet of
                      [] -> do
                          n <- use $ acNick . _Just
                          write h . render $ errNeedMoreParams n
                      (pass:_) -> acPassword ?= pass
        _ -> return ()

getAuthInfo :: Handle -> AuthState (Maybe (Username, Text))
getAuthInfo h = fix $ \f -> do
    line <- io $ hGetLine h
    logInfoIO line
    respond h =<< pktCommand $ parse line
    ac <- get
    case ac of
        AuthClient _ (Just u) (Just p) -> do
            uname <- use $ acUsername . _Just
            write h . render $ rplNotify uname "Fetching token..."
            io $ fmap (fmap (uname,)) $ getToken u p
        _ -> f

authFailure :: Handle -> AuthState ()
authFailure h = do
    uname <- use $ acUsername . _Just
    write h . render $ errPasswordMismatch uname
    acPassword .= Nothing

authSuccess :: Handle -> AuthState ()
authSuccess h = do
    uname <- use $ acUsername . _Just
    write h . render $ rplNotify uname "Got a token."

write :: MonadIO m => Handle -> Text -> m ()
write h s = do
    logInfoIO $ fromMaybe s $ T.stripSuffix "\r\n" s
    io $ hPutStr h s
