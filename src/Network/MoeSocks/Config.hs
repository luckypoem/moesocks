{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.Config where

import Network.MoeSocks.Constant
import Network.MoeSocks.Type

defaultMoeConfig :: MoeConfig
defaultMoeConfig = MoeConfig
  {
    _remote= "127.0.0.1"
  , _remotePort = 1190
  , _local = "127.0.0.1"
  , _localPort = 1090
  , _password = "moesocks"
  , _method = _DefaultMethod
  }


defaultMoeOptions :: MoeOptions
defaultMoeOptions = MoeOptions
  {
    _runningMode = DebugMode
  , _configFile = ""
  , _socks5Header = Strict
  } 
