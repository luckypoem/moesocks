{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.MoeSocks.Type where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word
import GHC.Generics
import System.Log.Logger
import qualified Data.Strict as S

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
  | IPv6_address [Word16]
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
  , _timeout :: Int
  , _tcpBufferSize :: Int -- in packets
  , _throttle :: Bool
  , _throttleSpeed :: Double
  , _obfuscationFlushBound :: Int -- should be greater then MTU
  }
  deriving (Show, Eq, Generic)

instance FromJSON MoeConfig
instance ToJSON MoeConfig

makeLenses ''MoeConfig

data RunningMode = RemoteMode | LocalMode | DebugMode
      deriving (Show, Eq)

data Verbosity = Normal | Verbose
      deriving (Show, Eq)

data Forward = Forward
  {
    _forwardLocalPort :: Port
  , _forwardRemoteHost :: Text
  , _forwardRemotePort :: Port
  }
  deriving (Show, Eq)


makeLenses ''Forward

data MoeOptions = MoeOptions
  {
    _runningMode :: RunningMode
  , _configFile :: Maybe Text
  , _verbosity :: Priority
  , _forwardTCP :: [Forward]
  , _forwardUDP :: [Forward]
  , _disableSocks5 :: Bool
  , _obfuscation :: Bool
  , _params :: [(Text, Value)]
  }
  deriving (Show, Eq)

makeLenses ''MoeOptions

type Cipher = S.Maybe ByteString -> IO ByteString 
type IV = ByteString
type CipherBuilder = IV -> IO Cipher

data CipherBox = CipherBox
  {
    _ivLength :: Int
  , _generateIV :: IO IV
  , _encryptBuilder :: CipherBuilder
  , _decryptBuilder ::  CipherBuilder
  }

makeLenses '' CipherBox


data Env = Env
  {
    _options :: MoeOptions
  , _config :: MoeConfig
  , _cipherBox :: CipherBox
  }

makeLenses ''Env


type MoeMonadT = ReaderT MoeOptions (ExceptT String IO)

