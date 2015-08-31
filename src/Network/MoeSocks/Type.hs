{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.MoeSocks.Type where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.IP
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
    TCP_IP_StreamConnection
  | TCP_IP_PortBinding
  | UDP_Port
  deriving (Show, Eq)

data AddressType = 
    IPv4_Address (Word8, Word8, Word8, Word8)
  | DomainName Text
  | IPv6_Address [Word16]
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
  , _fastOpen :: Bool
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
  , _forward_TCP :: [Forward]
  , _forward_UDP :: [Forward]
  , _disable_SOCKS5 :: Bool
  , _obfuscation :: Bool
  , _forbidden_IP :: [IPRange]
  , _listMethods :: Bool
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
  , _generate_IV :: IO IV
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

