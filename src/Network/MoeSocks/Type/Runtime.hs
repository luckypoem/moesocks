{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.MoeSocks.Type.Runtime where

import Control.Lens
import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Lens
import Data.Word
import Data.IP
import Network.MoeSocks.Type.Common
import Numeric (showHex)
import qualified Data.List as L
import qualified Data.Strict as S

data ClientGreeting = ClientGreeting
  {
    _authenticationMethods :: [Word8]
  }
  deriving (Show)

makeLenses ''ClientGreeting

data ConnectionType =
    TCP_IP_StreamConnection
--  | TCP_IP_PortBinding
  | UDP_Port
  deriving (Show, Eq)

data AddressType =
    IPv4_Address (Word8, Word8, Word8, Word8)
  | DomainName Text
  | IPv6_Address [Word16]
  deriving (Eq)

instance Show AddressType where
  show = showAddressType
    where
      showAddressType :: AddressType -> String
      showAddressType (IPv4_Address xs) = xs ^.. each . to show
                                             ^.. folding
                                                  (concat . L.intersperse ".")

      showAddressType (DomainName x)   = x ^. _Text
      showAddressType (IPv6_Address xs) = xs ^.. each . to (flip showHex "")
                                             ^.. folding
                                                  (concat . L.intersperse ":")
data ClientRequest = ClientRequest
  {
    _connectionType :: ConnectionType
  , _addressType :: AddressType
  , _portNumber :: Port
  }
  deriving (Show, Eq)

makeLenses ''ClientRequest


data Verbosity = Normal | Verbose
      deriving (Show, Eq)

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

data LocalServiceType =
      LocalService_TCP_Forward Forward
    | LocalService_UDP_Forward Forward
    | LocalService_SOCKS5 Int
    deriving (Show, Eq)

makePrisms ''LocalServiceType


data LocalService = LocalService
  {
    _localServiceHost :: Text
  , _localServiceRemoteHost :: Text
  , _localServiceRemotePort :: Int
  , _localServiceType :: LocalServiceType
  }
  deriving (Show, Eq)

makeLenses ''LocalService

data RemoteRelayType =
      Remote_TCP_Relay
    | Remote_UDP_Relay
    deriving (Show, Eq)

makePrisms ''RemoteRelayType


data RemoteRelay = RemoteRelay
  {
    _remoteRelayType :: RemoteRelayType
  , _remoteRelayHost :: Text
  , _remoteRelayPort :: Int
  }
  deriving (Show, Eq)

makeLenses ''RemoteRelay

data Job =
      RemoteRelayJob RemoteRelay
    | LocalServiceJob LocalService
    deriving (Show, Eq)

makePrisms ''Job

data Env = Env
  {
    _timeout :: Int
  , _tcpBufferSize :: Int -- in packets
  , _throttle :: Bool
  , _throttleSpeed :: Double
  , _obfuscationFlushBound :: Int -- should be greater then MTU
  , _fastOpen :: Bool
  , _socketOption_TCP_NOTSENT_LOWAT :: Bool
  , _obfuscation :: Bool
  , _forbidden_IPs :: [IPRange]
  , _denyList :: [IPRange]
  , _cipherBox :: CipherBox
  }

makeLenses ''Env


data Runtime = Runtime
  {
    _jobs :: [Job]
  , _env :: Env
  }

makeLenses ''Runtime

type MoeMonad = ExceptT String IO
