{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.Default where

import Control.Lens
import Network.MoeSocks.Encrypt (constCipherBox)
import Network.MoeSocks.Type
import System.Log.Logger
import qualified Network.MoeSocks.Type.Bootstrap.Config as C
import qualified Network.MoeSocks.Type.Bootstrap.Option as O
import qualified Data.Text as T
import Data.Text (Text)
import Data.IP
import Data.Text.Lens

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
  , C._forbidden_IPs =  [
                          "::1"
                        , "0.0.0.0/16"
                        , "10.0.0.0/8"
                        , "127.0.0.0/24"
                        , "192.168.0.0/16"
                        ]
  }


defaultOptions :: O.Options
defaultOptions = O.Options
  {
    O._runningMode = O.DebugMode
  , O._configFile = Nothing
  , O._verbosity = DEBUG
  , O._forward_TCPs = []
  , O._forward_UDPs = []
  , O._disable_SOCKS5 = False
  , O._obfuscation = False
  , O._listMethods = False
  , O._showDefaultConfig = False
  , O._params = []
  } 

parseForbidden_IPs :: [Text] -> [IPRange]
parseForbidden_IPs =
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
                                            & parseForbidden_IPs
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
