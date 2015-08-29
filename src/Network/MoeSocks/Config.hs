{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.Config where

import Network.MoeSocks.Type
import System.Log.Logger

defaultMoeConfig :: MoeConfig
defaultMoeConfig = MoeConfig
  {
    _remote= "0.0.0.0"
  , _remotePort = 8388 
  , _local = "127.0.0.1"
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


defaultMoeOptions :: MoeOptions
defaultMoeOptions = MoeOptions
  {
    _runningMode = DebugMode
  , _configFile = Nothing
  , _verbosity = DEBUG
  , _forward_TCP = []
  , _forward_UDP = []
  , _disableSocks5 = False
  , _obfuscation = False
  , _forbidden_IP = ["127.0.0.1", "0.0.0.0", "::1"]
  , _params = []
  } 
