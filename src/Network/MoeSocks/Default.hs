{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.Default where

import           Control.Lens
import           Control.Monad.Writer (execWriter, tell)
import           Data.IP (IPRange)
import           Data.Text (Text)
import           Data.Text.Lens
import           System.Log.Logger (Priority(DEBUG))
import qualified Data.Text as T

import           Network.MoeSocks.Encrypt (constCipherBox)
import           Network.MoeSocks.Type
import qualified Network.MoeSocks.Type.Bootstrap.Config as C
import qualified Network.MoeSocks.Type.Bootstrap.Option as O

defaultConfig :: C.Config
defaultConfig = C.Config
  {
    C._remoteHost = "0.0.0.0"
  , C._remotePort = 8388
  , C._localHost = "127.0.0.1"
  , C._localPort = 1080
  , C._password = "moesocks"
  , C._method = "aes-256-cfb"
  , C._timeout = 300
  , C._tcpBufferSize = 1 -- should be handeled by CoDel and ecn in the kernel
  , C._throttle = False
  , C._throttleSpeed = 8000 -- in Kilobytes
  , C._obfuscationFlushBound = 4096
  , C._fastOpen = False
  , C._socketOption_TCP_NOTSENT_LOWAT = True
  , C._forbidden_IPs = _reserved_IP_Addresses
  }

  where
    _reserved_IP_Addresses :: [Text]
    _reserved_IP_Addresses = execWriter $ do
      let ip = tell . pure

      -- https://en.wikipedia.org/wiki/Reserved_IP_addresses
      ip "0.0.0.0/8"
      ip "10.0.0.0/8"
      ip "100.64.0.0/10"
      ip "127.0.0.0/8"
      ip "169.254.0.0/16"
      ip "172.16.0.0/12"
      ip "192.0.0.0/24"
      ip "192.0.2.0/24"
      ip "192.88.99.0/24"
      ip "192.168.0.0/16"
      ip "198.18.0.0/15"
      ip "198.51.100.0/24"
      ip "203.0.113.0/24"
      ip "224.0.0.0/4"
      ip "240.0.0.0/4"
      ip "255.255.255.255/32"

      ip "::/128"
      ip "::1/128"
      ip "::ffff:0:0/96"
      ip "100::/64"
      ip "64:ff9b::/96"
      ip "2001::/32"
      ip "2001:10::/28"
      ip "2001:20::/28"
      ip "2001:db8::/32"
      ip "2002::/16"
      ip "fc00::/7"
      ip "fe80::/10"
      ip "ff00::/8"


defaultOptions :: O.Options
defaultOptions = O.Options
  {
    O._runningMode = O.DebugMode
  , O._configFile = mempty
  , O._verbosity = DEBUG
  , O._forward_TCPs = []
  , O._forward_UDPs = []
  , O._disable_SOCKS5 = False
  , O._obfuscation = False
  , O._listMethods = False
  , O._showDefaultConfig = False
  , O._params = []
  , O._denyList = mempty
  }

parseIPRangeList :: [Text] -> [IPRange]
parseIPRangeList =
                    toListOf $ each
                              . to T.strip
                              . _Text
                              . _Show


defaultEnv :: Env
defaultEnv =
  let _c = defaultConfig
      _o = defaultOptions
  in
  Env
  {
      _timeout                        = _c ^. C.timeout
    , _tcpBufferSize                  = _c ^. C.tcpBufferSize
    , _throttle                       = _c ^. C.throttle
    , _throttleSpeed                  = _c ^. C.throttleSpeed
    , _obfuscationFlushBound          = _c ^. C.obfuscationFlushBound
    , _fastOpen                       = _c ^. C.fastOpen
    , _socketOption_TCP_NOTSENT_LOWAT = _c ^. C.socketOption_TCP_NOTSENT_LOWAT
    , _obfuscation                    = _o ^. O.obfuscation
    , _forbidden_IPs                  = _c ^. C.forbidden_IPs
                                            & parseIPRangeList
    , _denyList                       = []
    {-, _config = defaultConfig-}
    , _cipherBox = let (a,b,c,d) = constCipherBox in CipherBox a b c d
  }

defaultRuntime :: Runtime
defaultRuntime =
  Runtime
    {
      _jobs = []
    , _env = defaultEnv
    }
