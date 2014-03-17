{-# LANGUAGE OverloadedStrings #-}

module StubDamn where

import Control.Monad
import Control.Monad.Fix
import qualified Data.ByteString as B
import Data.Machine hiding (pass)
import Melvin.Prelude hiding (show)
import Network
import Text.Damn.Packet hiding (render)

runStubDamn = do
    sock <- listenOn (PortNumber 3900)
    (h, _, _) <- accept sock
    runT_ $ reading h 8192
         ~> endingBy "\0"
         ~> pass (liftIO . print)
         ~> auto (parse . cleanup)
         ~> handler h
    where cleanup m = if "\n" `B.isSuffixOf` m
                          then cleanup (B.init m)
                          else m

handler h = construct $ fix $ \f -> do
    m <- await
    case m of
        Left e -> error $ show e
        Right p -> do
            case pktCommand p of
                "dAmnClient" -> liftIO $ B.hPutStr h "dAmnServer 0.3\n\0"
                p -> error $ show p
            f
