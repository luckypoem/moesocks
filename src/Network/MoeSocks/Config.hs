{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Network.MoeSocks.Config where

import Control.Lens
import Data.Text (Text)
import "cipher-aes" Crypto.Cipher.AES
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Prelude ()
import Air.Env 
import Control.Lens
import Data.Monoid
import qualified Data.Text.Strict.Lens as TS

import Network.MoeSocks.Type
import Network.MoeSocks.Constant


defaultMoeConfig :: MoeConfig
defaultMoeConfig = MoeConfig
  {
    _server = "localhost"
  , _serverPort = 1190
  , _local = "0.0.0.0"
  , _localPort = 1090
  , _password = "moesocks"
  , _method = "none"
  }


aesKey :: MoeConfig -> AES
aesKey _config =
  let __password = 
        _config ^. password & review TS.utf8 :: ByteString

      _key = S.take _KeySize - __password <> S.pack (replicate _KeySize 0)
  in
  initAES _key
