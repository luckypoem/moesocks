{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.Constant where

import Data.Word

_No_authentication :: Word8
_No_authentication = 0
              
_Request_Granted :: Word8
_Request_Granted = 0

_ReservedByte :: Word8
_ReservedByte = 0

_SpaceCode :: Word8
_SpaceCode = 32

_PacketLength :: Int
_PacketLength = 4096


_NoThrottle :: Maybe a
_NoThrottle = Nothing
