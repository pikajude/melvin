{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Melvin.Client.Auth (
    authenticate
) where

import           Control.Monad.Catch
import           Control.Monad.State
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import           Melvin.Chatrooms hiding (render)
import           Melvin.Client.Packet
import           Melvin.Prelude hiding   (get)
import           Melvin.Token
import           Melvin.Types            (Chatroom)

data AuthClient = AuthClient
        { _acNick     :: Maybe Text
        , _acUsername :: Maybe Text
        , _acPassword :: Maybe Text
        , _acJoinlist :: S.Set Chatroom
        } deriving Show

makeLenses ''AuthClient

type Username = Text
type AuthState m = StateT AuthClient m

authenticate :: (MonadCatch m, LogIO m) => Handle -> m (Either IOError (Username, Text, S.Set Chatroom))
authenticate h = handleIOError (return . Left) $
    (`evalStateT` AuthClient Nothing Nothing Nothing mempty) . fix $ \f -> do
        ai <- getAuthInfo h
        case ai of
            Nothing -> authFailure h >> f
            Just t -> do
                authSuccess h
                return $ Right t

greet :: LogIO m => Handle -> Packet -> AuthState m ()
greet h Packet {
      pktArguments = (nick:_)
    } = do
    acNick .= Just nick
    write h $ [st|:chat.deviantart.com 001 %s :Welcome to dAmn %s!%s@chat.deviantart.com\r\n|] nick nick nick
    write h $ [st|:chat.deviantart.com 002 %s :Your host is chat.deviantart.com, running dAmnServer 0.3\r\n|] nick
    write h $ [st|:chat.deviantart.com 004 %s chat.deviantart.com dAmnServer0.3 qov i\r\n|] nick
    write h $ [st|:chat.deviantart.com 005 %s PREFIX=(qov)~@+\r\n|] nick
greet h _ = write h . render $ errNoNicknameGiven "stupid"

respond :: LogIO m => Handle -> Text -> Packet -> AuthState m ()
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
        "JOIN" -> case pktArguments packet of
                      [] -> do
                          n <- use $ acNick . _Just
                          write h . render $ errNeedMoreParams n
                      (room:_) | Just r <- toChatroom room
                          -> acJoinlist %= S.insert r
                      (r:_) -> do
                          n <- use $ acNick . _Just
                          write h . render $ errNoSuchChannel n r
        _ -> return ()

getAuthInfo :: LogIO m => Handle -> AuthState m (Maybe (Username, Text, S.Set Chatroom))
getAuthInfo h = fix $ \f -> do
    line <- liftIO $ hGetLine h
    $logDebug line
    respond h =<< pktCommand $ parse line
    ac <- get
    case ac of
        AuthClient _ (Just u) (Just p) js -> do
            uname <- use $ acUsername . _Just
            write h . render $ rplNotify uname "Fetching token..."
            liftIO $ fmap (fmap (uname, , js)) $ getToken u p
        _ -> f

authFailure :: LogIO m => Handle -> AuthState m ()
authFailure h = do
    uname <- use $ acUsername . _Just
    write h . render $ errPasswordMismatch uname
    acPassword .= Nothing

authSuccess :: LogIO m => Handle -> AuthState m ()
authSuccess h = do
    uname <- use $ acUsername . _Just
    write h . render $ rplNotify uname "Got a token."

write :: (MonadLogger m, MonadIO m) => Handle -> Text -> m ()
write h s = do
    $logDebug $ fromMaybe s $ T.stripSuffix "\r\n" s
    liftIO $ hPutStr h s
