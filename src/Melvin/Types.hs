{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Melvin.Types (
  Chatroom(..),

  Privclass,
  Symbol,
  pcLevel,
  pcTitle,
  pcSymbol,
  mkPrivclass,
  asMode,

  User,
  userMember,
  userPrivclass,
  userIcon,
  userSymbol,
  userRealname,
  userGpc,
  userJoinCount,
  mkUser,
  renderUser,

  ClientSettings(..),
  clientWriteLock,
  serverWriteLock,
  username,
  token,
  serverMVar,
  clientThreadId,
  serverThreadId,
  retryWait,

  writeClient,
  writeServer,
  killClient,
  killServer,

  ClientState(..),
  loggedIn,
  joinList,
  joining,
  privclasses,
  users,

  getState,
  getsState,
  modifyState,
  putState,

  ClientT, StM
) where

import           Control.Arrow
import           Control.Concurrent.Lifted
import           Control.Concurrent.Async.Lifted
import qualified Control.Exception as E
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Trans.Control
import qualified Data.ByteString as B
import           Data.Map                 (Map)
import           Data.Set
import           Data.Text
import           Melvin.Client.Packet
import           Melvin.Exception
import           Melvin.Prelude hiding    (cons)
import qualified Text.Damn.Packet as D

data Chatroom =
          Chatroom Text
        | PrivateChat Text
        deriving (Eq, Ord, Show)

data Symbol = Founder | Op | Voice | None
            deriving (Eq, Ord, Show)

data Privclass = Privclass
        { pcLevel  :: Int
        , pcTitle  :: Text
        , pcSymbol :: Symbol
        } deriving (Eq, Ord, Show)

mkPrivclass :: Int -> Text -> Privclass
mkPrivclass n t = Privclass n t (toSymbol n)
    where toSymbol s
            | s == 99 = Founder
            | s >= 70 = Op
            | s >= 35 = Voice
            | otherwise = None

data User = User
        { _userMember     :: Text
        , _userPrivclass  :: Maybe Privclass
        , _userIcon       :: Int
        , _userSymbol     :: Char
        , _userRealname   :: Text
        , _userGpc        :: Text
        , _userJoinCount :: Int
        } deriving (Eq, Ord, Show)

makeLenses ''User

mkUser :: Map Text Privclass -> Text -> Text -> Int -> Char -> Text -> Text -> User
mkUser ps m p i s r g = User m (ps ^? ix p) i s r g 1

renderUser :: User -> Text
renderUser User { _userMember = m, _userPrivclass = pc } =
    case pcSymbol <$> pc of
        Nothing      -> m
        Just None    -> m
        Just Voice   -> cons '+' m
        Just Op      -> cons '@' m
        Just Founder -> cons '~' m

asMode :: Privclass -> Maybe Text
asMode pc = case pcSymbol pc of
                None -> Nothing
                Voice -> Just "v"
                Op -> Just "o"
                Founder -> Just "q"

data ClientState = ClientState
        { _loggedIn    :: Bool
        , _joinList    :: Set Chatroom
        , _joining     :: Set Text
        , _privclasses :: Map Chatroom (Map Text Privclass)
        , _users       :: Map Chatroom (Map Text User)
        }

makeLenses ''ClientState

data ClientSettings = ClientSettings
        { clientNumber          :: Integer
        , _clientHandle         :: Handle
        , _clientWriteLock      :: MVar ()
        , _serverWriteLock      :: MVar ()
        , _username             :: Text
        , _token                :: Text
        , _serverMVar           :: MVar Handle
        , _clientThreadId       :: MVar ThreadId
        , _serverThreadId       :: MVar ThreadId
        , _retryWait            :: Integer
        , _clientState          :: MVar ClientState
        }

makeLenses ''ClientSettings

type ClientT m = (MonadFix m, Functor m, MonadCatch m, MonadBaseControl IO m,
                  MonadState ClientSettings m, MonadLogger m, MonadIO m)

writeClient :: ClientT m => Packet -> m ()
writeClient text = do
    (mv, h) <- gets (view clientWriteLock &&& view clientHandle)
    bracket_
        (takeMVar mv)
        (putMVar mv ())
        ((liftIO (hPutStr h r) >> $logDebug (show r)) `catch` fallback)
    where fallback e = do
            $logError $ "(write failed) " ++ show r
            E.throw $ ClientSocketErr e
          r = render text

writeServer :: ClientT m => D.Packet -> m ()
writeServer text = do
    (mv, h) <- gets (view serverWriteLock &&& view serverMVar)
    bracket
        (takeMVar mv >> tryTakeMVar h)
        (\_ -> putMVar mv ())
        (\mHndl -> case mHndl of
            Nothing -> E.throw ServerNotConnected
            Just hndl -> (do
                putMVar h hndl
                liftIO $ B.hPutStr hndl (D.render text ++ "\n\0")
                $logDebug (show text)) `catch` fallback)
    where fallback e = do
            $logError $ "(write failed) " ++ show text
            E.throw $ ServerSocketErr e

killClient :: ClientT m => m ()
killClient = do
    ct <- use clientThreadId
    tid <- tryTakeMVar ct
    case tid of
        Nothing -> $logWarn "Client thread is already dead."
        Just t -> killThread t

killServer :: ClientT m => m ()
killServer = do
    ct <- use serverThreadId
    tid <- tryTakeMVar ct
    case tid of
        Nothing -> $logWarn "Server thread is already dead."
        Just t -> killThread t

modifyState :: ClientT m => (ClientState -> ClientState) -> m ()
modifyState f = do
    cs <- use clientState
    modifyMVar_ cs (return . f)

getState :: ClientT m => m ClientState
getState = do
    cs <- use clientState
    readMVar cs

getsState :: ClientT m => (ClientState -> a) -> m a
getsState f = do
    cs <- use clientState
    sta <- readMVar cs
    return $ f sta

putState :: ClientT m => ClientState -> m ()
putState v = do
    cs <- use clientState
    _ <- tryTakeMVar cs
    putMVar cs v
