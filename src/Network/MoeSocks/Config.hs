{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.MoeSocks.Config where

import "cipher-aes" Crypto.Cipher.AES
import Control.Lens
import Data.ByteString (ByteString)
import Network.MoeSocks.Constant
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
import qualified Data.Text.Strict.Lens as TS

defaultMoeConfig :: MoeConfig
defaultMoeConfig = MoeConfig
  {
    _remote= "127.0.0.1"
  , _remotePort = 1190
  , _local = "127.0.0.1"
  , _localPort = 1090
  , _password = "moesocks"
  {-, _method = "none"-}
  }


aesKey :: MoeConfig -> AES
aesKey _config =
  let __password = 
        _config ^. password & review TS.utf8 :: ByteString

      _key = clamp _KeySize __password 
  in
  initAES _key


defaultMoeOptions :: MoeOptions
defaultMoeOptions = MoeOptions
  {
    _runningMode = DebugMode
  , _configFile = ""
  } 
