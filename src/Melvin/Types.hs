{-# LANGUAGE FlexibleContexts #-}
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

  ClientT
) where

import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Control.Exception as E
import           Data.Map                 (Map)
import           Data.Set
import           Data.Text
import           Melvin.Client.Packet
import           Melvin.Exception
import           Melvin.Logger
import           Melvin.Prelude hiding    (cons)
import           Pipes.Safe
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
        , _clientThreadId       :: MVar (Async (Either SomeException ()))
        , _serverThreadId       :: MVar (Async (Either SomeException ()))
        , _retryWait            :: Integer
        , _clientState          :: MVar ClientState
        }

makeLenses ''ClientSettings

type ClientT = SafeT (StateT ClientSettings IO)

writeClient :: Packet -> ClientT ()
writeClient text = do
    (mv, h) <- lift $ gets (view clientWriteLock &&& view clientHandle)
    liftIO $ E.bracket_
        (takeMVar mv)
        (putMVar mv ())
        ((hPutStr h r >> logInfoIO r) `E.catch` fallback)
    where fallback e = do
            logWarningIO $ "(write failed) " ++ r
            E.throw $ ClientSocketErr e
          r = render text

writeServer :: D.Packet -> ClientT ()
writeServer text = do
    (mv, h) <- lift $ gets (view serverWriteLock &&& view serverMVar)
    liftIO $ E.bracket
        (takeMVar mv >> readMVar h)
        (\_ -> putMVar mv ())
        (\hndl -> (hPutStr hndl (D.render text ++ "\n\0") >> logInfoIO (show text)) `E.catch` fallback)
    where fallback e = do
            logWarningIO $ "(write failed) " ++ show text
            E.throw $ ServerSocketErr e

killClient :: ClientT ()
killClient = do
    ct <- lift $ use clientThreadId
    tid <- liftIO $ tryTakeMVar ct
    case tid of
        Nothing -> logWarning "Client thread is already dead."
        Just t -> liftIO $ cancel t

killServer :: ClientT ()
killServer = do
    ct <- lift $ use serverThreadId
    tid <- liftIO $ tryTakeMVar ct
    case tid of
        Nothing -> logWarning "Server thread is already dead."
        Just t -> liftIO $ cancel t

modifyState :: (ClientState -> ClientState) -> ClientT ()
modifyState f = do
    cs <- lift $ use clientState
    liftIO $ modifyMVar_ cs (return . f)

getState :: ClientT ClientState
getState = do
    cs <- lift $ use clientState
    liftIO $ readMVar cs

getsState :: (ClientState -> a) -> ClientT a
getsState f = do
    cs <- lift $ use clientState
    sta <- liftIO $ readMVar cs
    return $ f sta

putState :: ClientState -> ClientT ()
putState v = do
    cs <- lift $ use clientState
    _ <- liftIO $ tryTakeMVar cs
    liftIO $ putMVar cs v
