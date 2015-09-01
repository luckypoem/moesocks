{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.Config where

import Network.MoeSocks.Type
import System.Log.Logger

defaultConfig :: Config
defaultConfig = Config
  {
    _remoteAddress = "0.0.0.0"
  , _remotePort = 8388 
  , _localAddress = "127.0.0.1"
  , _localPort = 1080
  , _password = "moesocks"
  , _method = "aes-256-cfb"
  , _timeout = 300
  , _tcpBufferSize = 256
  , _throttle = False
  , _throttleSpeed = 8000 -- in Kilobytes
  , _obfuscationFlushBound = 4096
  , _fastOpen = False
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
