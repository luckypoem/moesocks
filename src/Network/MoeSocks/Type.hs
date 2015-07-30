{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.MoeSocks.Type where

import Control.Lens
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word
import GHC.Generics
import System.Log.Logger

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
    IPv4_address (Word8, Word8, Word8, Word8)
  | Domain_name Text
  | IPv6_address (Word32, Word32, Word32, Word32)
  deriving (Show, Eq)

type Port = Int

data ClientRequest = ClientRequest
  {
    _connectionType :: ConnectionType
  , _addressType :: AddressType
  , _portNumber :: Port
  }
  deriving (Show, Eq)

makeLenses ''ClientRequest
 
data MoeConfig = MoeConfig
  {
    _remote :: Text
  , _remotePort :: Int
  , _local :: Text
  , _localPort :: Int
  , _password :: Text
  , _method :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON MoeConfig
instance ToJSON MoeConfig

makeLenses ''MoeConfig

data RunningMode = RemoteMode | LocalMode | DebugMode
      deriving (Show, Eq)

data Verbosity = Normal | Verbose
      deriving (Show, Eq)

data MoeOptions = MoeOptions
  {
    _runningMode :: RunningMode
  , _configFile :: Text
  , _verbosity :: Priority
  }
  deriving (Show, Eq)

makeLenses ''MoeOptions


type Cipher = ByteString -> IO ByteString 
