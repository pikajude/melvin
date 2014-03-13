-- | Provides convenience functions for manipulating dAmn packets.
module Text.Damn.Packet (
  -- * The Packet datatype
  Packet(..),
  Arguments,

  -- ** Subpackets
  pktSubpacket,
  pktSubpacket',

  -- ** Packet lenses
  pktCommandL,
  pktParameterL,
  pktArgsL,
  pktBodyL,
  pktSubpacketL,

  -- * Parsing
  parse,
  parse',

  -- * Rendering packets
  render
) where

import Text.Damn.Packet.Parser
import Text.Damn.Packet.Internal
