{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.Config where

import Network.MoeSocks.Constant
import Network.MoeSocks.Type
import System.Log.Logger

defaultMoeConfig :: MoeConfig
defaultMoeConfig = MoeConfig
  {
    _remote= "127.0.0.1"
  , _remotePort = 1190
  , _local = "127.0.0.1"
  , _localPort = 1090
  , _password = "moesocks"
  , _method = _DefaultMethod
  , _timeout = 300
  , _tcpBufferSizeInPacket = 16
  }


defaultMoeOptions :: MoeOptions
defaultMoeOptions = MoeOptions
  {
    _runningMode = DebugMode
  , _configFile = ""
  , _verbosity = DEBUG
  , _localForwarding = []
  } 
