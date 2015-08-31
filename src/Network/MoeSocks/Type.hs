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
import Data.Monoid
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


data Config = Config
  {
    _remoteAddress :: Text
  , _remotePort :: Int
  , _localAddress :: Text
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

instance FromJSON Config
instance ToJSON Config

makeLenses ''Config

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

data Options = Options
  {
    _runningMode :: RunningMode
  , _configFile :: Maybe Text
  , _verbosity :: Priority
  , _forward_TCPs :: [Forward]
  , _forward_UDPs :: [Forward]
  , _disable_SOCKS5 :: Bool
  , _obfuscation :: Bool
  , _forbidden_IPs :: [IPRange]
  , _listMethods :: Bool
  , _params :: [(Text, Value)]
  }
  deriving (Show, Eq)

makeLenses ''Options

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

makeLenses ''CipherBox

data LocalRelayType =
      Local_TCP_Relay Forward
    | Local_UDP_Relay Forward
    | Local_SOCKS_Relay Int
    deriving (Show, Eq)

makePrisms ''LocalRelayType

data LocalRelay = LocalRelay
  {
    _localRelayType :: LocalRelayType
  , _localRelayAddress :: Text
  , _localRelayRemoteAddress :: Text
  , _localRelayRemotePort :: Int
  }
  deriving (Show, Eq)

makeLenses ''LocalRelay

data RemoteRelayType =
      Remote_TCP_Relay
    | Remote_UDP_Relay
    deriving (Show, Eq)

makePrisms ''RemoteRelayType

data RemoteRelay = RemoteRelay
  {
    _remoteRelayType :: RemoteRelayType
  , _remoteRelayAddress :: Text
  , _remoteRelayPort :: Int
  }
  deriving (Show, Eq)

makeLenses ''RemoteRelay

data Runtime = Runtime
  {
    _localRelays :: [LocalRelay]
  , _remoteRelays :: [RemoteRelay]
  }
  deriving (Show, Eq)

instance Monoid Runtime where
  mempty = Runtime [] []
  Runtime x y `mappend` Runtime x' y' = Runtime (x <> x') (y <> y')
          
makeLenses ''Runtime

data Env = Env
  {
    _options :: Options
  , _config :: Config
  , _cipherBox :: CipherBox
  }

makeLenses ''Env


type MoeMonadT = ReaderT Options (ExceptT String IO)

