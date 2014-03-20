module Melvin.Client (loop) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fix
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import           Melvin.Chatrooms
import           Melvin.Client.Packet hiding (render)
import qualified Melvin.Damn.Actions as Damn
import           Melvin.Prelude
import           Melvin.Types

handler :: ClientT m => SomeException -> m ()
handler ex = do
    $logError $ show (ex :: SomeException)
    killServer
    throwM ex

loop :: ClientT m => Handle -> m ()
loop hndl = bracket
    (return hndl)
    (liftIO . hClose)
    (\h -> handle handler $ runT_ $
        reading h 512
            ~> endingBy "\r\n"
            ~> pass ($logDebug . show)
            ~> auto parse
            ~> handleClient)

handleClient :: (Category k, ClientT m) => MachineT m (k Packet) a
handleClient = construct $ fix $ \f -> do
    p <- await
    continue <- case M.lookup (pktCommand p) responses of
        Nothing -> do
            $logInfo $ [st|Unhandled packet from client: %?|] p
            return False
        Just callback -> do
            sta <- get
            lift $ callback p sta
    when continue f

-- | Big ol' list of callbacks!
type Callback m = Packet -> ClientSettings -> m Bool

responses :: ClientT m => M.Map Text (Callback m)
responses = M.fromList [ ("QUIT", res_quit)
                       , ("PING", res_ping)
                       , ("WHO",  ignore)
                       , ("PONG", ignore)
                       , ("USER", ignore)
                       , ("NICK", ignore)
                       , ("MODE", res_mode)
                       , ("JOIN", res_join)
                       , ("PART", res_part)
                       , ("PRIVMSG", res_privmsg)
                       ] where
    ignore _ _ = return True

res_quit :: ClientT m => Callback m
res_quit _ sta = do
    $logDebug $ [st|Client #%d quit|] (clientNumber sta)
    killServer
    return False

res_ping :: ClientT m => Callback m
res_ping Packet { pktArguments = args } _ = do
    writeClient $ cmdPong args
    return True

res_mode :: ClientT m => Callback m
res_mode p _ = do
    $logInfo $ [st|Received mode command, should handle: %?|] p
    return True

res_join :: ClientT m => Callback m
res_join Packet { pktArguments = a } sta = do
    case a of
        [] -> writeClient $ errNeedMoreParams (sta ^. username)
        (rooms:_) -> forM_ (T.splitOn "," rooms) $ \r ->
            case toChatroom r of
                Nothing -> writeClient $ errNoSuchChannel (sta ^. username) r
                Just c -> do
                    l <- getsState $ view loggedIn
                    if l
                       then writeServer =<< Damn.join c
                       else modifyState (joinList %~ S.insert c)
    return True

res_part :: ClientT m => Callback m
res_part Packet { pktArguments = a } sta = do
    case a of
        [] -> writeClient $ errNeedMoreParams (sta ^. username)
        (room:_) -> case toChatroom room of
            Nothing -> writeClient $ errNoSuchChannel (sta ^. username) room
            Just c -> writeServer =<< Damn.part c
    return True

res_privmsg :: ClientT m => Callback m
res_privmsg Packet { pktArguments = (room:msg) } sta = do
    case toChatroom room of
        Nothing -> writeClient $ errNoSuchChannel (sta ^. username) room
        Just r -> case msg of
                      [] -> writeClient $ errNeedMoreParams (sta ^. username)
                      (m:_) -> case T.stripPrefix "\1ACTION " m of
                                   Just ac -> writeServer =<< Damn.action r (T.init ac)
                                   Nothing -> writeServer =<< Damn.msg r m
    return True
res_privmsg _ sta = writeClient (errNeedMoreParams (sta ^. username)) >> return True

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
