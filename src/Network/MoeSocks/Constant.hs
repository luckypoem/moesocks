module Network.MoeSocks.Constant where

import Data.Word
import GHC.Int (Int64)

_No_authentication :: Word8
_No_authentication = 0
              
_Request_Granted :: Word8
_Request_Granted = 0

_ReservedByte :: Word8
_ReservedByte = 0

_KeySize :: Int
_KeySize = 32

_BlockSize :: Int
_BlockSize = 16

-- packetSize should be less then 256
-- since only one byte, the first, is holding the packetSize 
_PacketSize :: Int64
_PacketSize = 256

_SpaceCode :: Word8
_SpaceCode = 32


