{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

module Network.MoeSocks.Type where

import Control.Lens
import Data.Word
import Data.ByteString (ByteString)
import Data.Text (Text)

data ClientGreeting = ClientGreeting
  {
    _authenticationMethods :: [Word8]
  }
  deriving (Show)

makeLenses ''ClientGreeting

data ConnectionType =
    TCP_IP_stream_connection
  | TCP_IP_port_binding
  | UDP_port
  deriving (Show, Eq)

data AddressType = 
    IPv4_address [Word8]
  | Domain_name ByteString
  | IPv6_address [Word8]
  deriving (Show, Eq)


data ClientRequest = ClientRequest
  {
    _connectionType :: ConnectionType
  , _addressType :: AddressType
  , _portNumber :: (Word8, Word8)
  }
  deriving (Show)

makeLenses ''ClientRequest
 
data MoeConfig = MoeConfig
  {
    _server :: Text
  , _serverPort :: Int
  , _local :: Text
  , _localPort :: Int
  , _password :: Text
  , _method :: Text
  }
  deriving (Show, Eq)

makeLenses ''MoeConfig
