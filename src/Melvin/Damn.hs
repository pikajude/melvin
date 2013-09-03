module Melvin.Damn (
  packetStream,
  responder
) where

import           Control.Arrow
import           Control.Concurrent
import           Control.Exception           (throwIO)
import           Control.Lens hiding         (index)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Proxy
import           Control.Proxy.Safe
import           Control.Proxy.Trans.State
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Melvin.Client.Packet hiding (Packet(..), parse)
import           Melvin.Exception
import           Melvin.Logger
import           Melvin.Prelude
import           Melvin.Types
import           System.IO hiding            (isEOF, putStrLn)
import           System.IO.Error
import           Text.Damn.Packet

hGetTillNull :: Handle -> IO Text
hGetTillNull h = do
    ready <- hWaitForInput h 180000
    if ready
        then do
            ch <- hGetChar h
            if ch == '\0'
                then return mempty
                else fmap (T.cons ch) $ hGetTillNull h
        else throwIO $ mkIOError eofErrorType "read timeout" (Just h) Nothing

handler :: SomeException
        -> ClientP a' a b' b SafeIO ()
handler ex = do
    uname <- liftP $ gets (view username)
    writeClient $ rplNotify uname $ formatS "Error when communicating with dAmn: {}" [show ex]
    if isRetryable ex
        then writeClient $ rplNotify uname "Trying to reconnect..."
        else do
            writeClient $ rplNotify uname "Unrecoverable error. Disconnecting..."
            killClient
    throw ex

packetStream :: Integer -> MVar Handle -> () -> Producer ClientP Packet SafeIO ()
packetStream index mv () = bracket id
    (do hndl <- readMVar mv
        auth hndl
        return hndl)
    (\h -> do
        logInfoIO $ formatS "Client #{} disconnected from dAmn." [show index]
        hClose h)
    (\h -> handle handler $ fix $ \f -> do
        isEOF <- tryIO $ hIsEOF h
        isClosed <- tryIO $ hIsClosed h
        when (isEOF || isClosed) $ throw ServerDisconnect
        line <- tryIO $ hGetTillNull h
        case parse $ cleanup line of
            Left err -> throw $ ServerNoParse err line
            Right pk -> do
                respond pk
                f)
    where cleanup m = fromMaybe m $ T.stripSuffix "\n" m

responder :: () -> Consumer ClientP Packet SafeIO ()
responder () = fix $ \f -> do
    p <- request ()
    case M.lookup (pktCommand p) responses of
        Nothing -> logInfo $ formatS "Unhandled packet from damn: {}" [show p]
        Just callback -> callback p
    unless (pktCommand p == "disconnect") f

auth :: Handle -> IO ()
auth h = hprint h "dAmnClient 0.3\nagent=melvin 0.1\n\0" ()


-- | Big ol' list of callbacks!
type Callback = Packet -> Consumer ClientP Packet SafeIO ()

responses :: M.Map Text Callback
responses = M.fromList [ ("dAmnServer", res_dAmnServer)
                       , ("login", res_login)
                       ]

res_dAmnServer :: Callback
res_dAmnServer _ = do
    (num, (u, tok)) <- liftP . gets $ clientNumber &&& view username &&& view token
    logInfo $ formatS "Client #{} handshook successfully." [num]
    writeServer $ formatS "login {}\npk={}\n" [u, tok]

res_login :: Callback
res_login Packet { pktArgs = args } = do
    uname <- liftP $ gets (view username)
    case args ^. ix "e" of
        "ok" -> do
            writeClient $ rplNotify uname "Authenticated successfully."
            liftP $ modify (loggedIn .~ True)
        x -> do
            writeClient $ rplNotify uname "Authentication failed!"
            throw $ AuthenticationFailed x

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
