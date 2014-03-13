{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

-- | Internal functions for Packets.
module Text.Damn.Packet.Internal (
  Packet(..),
  Arguments,
  pktCommandL,
  pktParameterL,
  pktArgsL,
  pktBodyL
) where

import Control.Applicative
import Control.DeepSeq
import Data.ByteString
import Data.Data
import Data.Map            (Map)
import Data.Text           (Text)
import Prelude

-- | A type synonym--because pressing spacebar is pretty irritating.
type Arguments = Map Text Text

lens :: Functor f => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}

-- | Represents a dAmn packet.
--
-- Packets are comprised of a command, which is mandatory, and three other
-- optional elements: a \"parameter\", an argument list, and a body. Any
-- combination of the latter three (or none of them) is valid, so the
-- parser is fairly lenient.
--
-- A packet with all four elements will look something like this:
--
-- @
--property chat:SomeChatroom
--p=propertyName
--by=name of setter
--ts=timestamp
--
--value of property
-- @
--
-- Parsing this results in the packet:
--
-- @
--'Packet' { 'pktCommand' = \"property\"
--       , 'pktParameter' = 'Just' \"chat:SomeChatroom\"
--       , 'pktArgs' = 'fromList' [(\"p\",\"propertyName\"),(\"by\",\"name of setter\"),(\"ts\",\"timestamp\")]
--       , 'pktBody' = 'Just' \"value of property\"
--       }
-- @
data Packet = Packet { pktCommand   :: Text
                     , pktParameter :: Maybe Text
                     , pktArgs      :: Arguments
                     , pktBody      :: Maybe ByteString
                     } deriving (Eq, Data, Show, Typeable)

instance NFData Packet where
   rnf (Packet a b c d) = rnf a `seq`
                          rnf b `seq`
                          rnf c `seq`
                          rnf d `seq`
                          ()

-- | A lens on 'pktCommand'.
pktCommandL :: Functor f
            => (Text -> f Text)
            -> Packet -> f Packet
pktCommandL = lens pktCommand (\pk b -> pk { pktCommand = b })
{-# INLINE pktCommandL #-}

-- | A lens on 'pktParameter'.
pktParameterL :: Functor f
              => (Maybe Text -> f (Maybe Text))
              -> Packet -> f Packet
pktParameterL = lens pktParameter (\pk b -> pk { pktParameter = b })
{-# INLINE pktParameterL #-}

-- | A lens on 'pktArgs'.
pktArgsL :: Functor f
         => (Arguments -> f Arguments)
         -> Packet -> f Packet
pktArgsL = lens pktArgs (\pk b -> pk { pktArgs = b })
{-# INLINE pktArgsL #-}

-- | A lens on 'pktBody'.
pktBodyL :: Functor f
         => (Maybe ByteString -> f (Maybe ByteString))
         -> Packet -> f Packet
pktBodyL = lens pktBody (\pk b -> pk { pktBody = b })
{-# INLINE pktBodyL #-}
