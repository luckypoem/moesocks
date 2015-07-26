{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Network.MoeSocks.Config where

import Control.Lens
import "cipher-aes" Crypto.Cipher.AES
import Data.ByteString (ByteString)
import qualified Data.Text.Strict.Lens as TS

import Network.MoeSocks.Type
import Network.MoeSocks.Constant

import Network.MoeSocks.Helper

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
