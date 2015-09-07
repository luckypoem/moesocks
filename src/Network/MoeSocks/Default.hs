{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.Default where

import Network.MoeSocks.Type
import Network.MoeSocks.Type.Bootstrap.Option
import qualified Network.MoeSocks.Type.Bootstrap.Config as C
import System.Log.Logger
import Control.Lens

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
  , C._tcpBufferSize = 256
  , C._throttle = False
  , C._throttleSpeed = 8000 -- in Kilobytes
  , C._obfuscationFlushBound = 4096
  , C._fastOpen = False
  , C._socketOption_TCP_NOTSENT_LOWAT = True
  }


defaultOptions :: Options
defaultOptions = Options
  {
    _runningMode = DebugMode
  , _configFile = Nothing
  , _verbosity = DEBUG
  , _forward_TCPs = []
  , _forward_UDPs = []
  , _disable_SOCKS5 = False
  , _obfuscation = False
  , _forbidden_IPs = ["127.0.0.1", "0.0.0.0", "::1"]
  , _listMethods = False
  , _params = []
  } 

defaultRuntime :: Runtime
defaultRuntime =
  let _c = defaultConfig
  in
  Runtime
    {
      _jobs                           = []
    , _timeout                        = _c ^. C.timeout
    , _tcpBufferSize                  = _c ^. C.tcpBufferSize
    , _throttle                       = _c ^. C.throttle
    , _throttleSpeed                  = _c ^. C.throttleSpeed
    , _obfuscationFlushBound          = _c ^. C.obfuscationFlushBound 
    , _fastOpen                       = _c ^. C.fastOpen
    , _socketOption_TCP_NOTSENT_LOWAT = _c ^. C.socketOption_TCP_NOTSENT_LOWAT
    }
