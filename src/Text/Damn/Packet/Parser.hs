-- | Parsing Packets.
module Text.Damn.Packet.Parser (
  Packet(..),
  parse, parse',
  render,
  pktSubpacket,
  pktSubpacket',
  pktSubpacketL
) where

import           Prelude                   hiding (null)
import           Control.Applicative       ((<$>), (*>), liftA3, pure)
import           Control.Arrow             (second)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import           Data.Char
import qualified Data.Map as M
import           Data.Monoid
import           Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import           Data.Word (Word8)
import           Text.Damn.Packet.Internal

-- | A lens on 'pktSubpacket''.
pktSubpacketL :: Functor f => (Maybe Packet -> f (Maybe Packet)) -> Packet -> f Packet
pktSubpacketL afb s = setter s <$> afb (getter s)
    where getter = pktSubpacket'
          setter pkt m = pkt { pktBody = render <$> m }
{-# INLINE pktSubpacketL #-}

-- | Due to the way dAmn packets are designed, it's not possible to
-- unambiguously determine whether a packet has a subpacket or just a body.
-- Thus you will need to request a subpacket yourself.
pktSubpacket :: Packet -> Either String Packet
pktSubpacket Packet { pktBody = b } =
        case b of Nothing -> Left "Parent packet has no body!"
                  Just pk -> parse pk
{-# INLINE pktSubpacket #-}

-- | Use when you don't care about the reason for parse failure.
pktSubpacket' :: Packet -> Maybe Packet
pktSubpacket' p = case pktSubpacket p of
                      Right pk -> Just pk
                      _ -> Nothing
{-# INLINE pktSubpacket' #-}

-- | 'render' converts a packet back into the dAmn text format.
-- This is used by 'pktSubpacketL' to fulfill the lens laws, but you might
-- find it useful if you want to write packets to dAmn.
render :: Packet -> B.ByteString
render (Packet cmd prm args b) =
        encodeUtf8 cmd
     <> maybe "" ((" " <>) . encodeUtf8) prm
     <> M.foldrWithKey
        (\k v m -> "\n" <> encodeUtf8 k <> "=" <> encodeUtf8 v <> m) "" args
     <> maybe "" ("\n\n" <>) b
{-# INLINE render #-}

-- | Parse some text, providing a packet or the reason for parse failure.
parse :: B.ByteString -> Either String Packet
parse str = if B.null body
                then packet
                else addBody packet
    where adjustedStr = accountForLoginSpace str
          (header, body) = second (B.drop 2) $ B.breakSubstring "\n\n" adjustedStr
          packet = A.parseOnly headP header
          addBody (Right s) = Right $ s { pktBody = Just body }
          addBody l = l

-- | Parse some text, discarding any failure message.
parse' :: B.ByteString -> Maybe Packet
parse' s = case parse s of
               Right pk -> Just pk
               _ -> Nothing
{-# INLINE parse' #-}

headP :: A.Parser Packet
headP = do
    cmd <- decodeUtf8 <$> A.takeWhile1 (not . isSpace . chr8)
    prm <- A.option Nothing paramP
    args <- argsP
    return $ Packet cmd (decodeUtf8 <$> prm) args Nothing
    where
        paramP = fmap Just (char ' ' *> A.takeWhile1 (not . isSpace . chr8)) A.<?> "parameter"
        argsP = do
            ch <- A.option Nothing (Just <$> char '\n')
            case ch of
                Just _ -> liftA3 M.insert
                              (decodeUtf8 <$> A.takeWhile1 (/= ord8 '='))
                              (char '=' >> fmap decodeUtf8 (A.takeWhile (/= ord8 '\n')))
                              argsP
                _      -> pure mempty

char :: Char -> A.Parser Word8
char = A.word8 . ord8

ord8 :: Char -> Word8
ord8 = toEnum . fromEnum

chr8 :: Word8 -> Char
chr8 = toEnum . fromEnum

-- | The login packet looks like this:
--
-- @
--login username
--e=event
--
--symbol=~
--realname=Some Name
-- @
--
-- That is, with an extra space after the \"event\" argument. Since this is
-- unintuitive behavior and no other packet behaves this way, the parser has
-- a special case for login packets where it will eliminate the extra newline.
accountForLoginSpace :: B.ByteString -> B.ByteString
accountForLoginSpace s = if "login " `B.isPrefixOf` s
                             then replace "\n\n" "\n" s
                             else s
    where replace find rep ss = case B.breakSubstring find ss of
              (n, b) | B.null b -> n
              (a, b) -> a <> rep <> replace find rep (B.drop (B.length find) b)
{-# INLINE accountForLoginSpace #-}
