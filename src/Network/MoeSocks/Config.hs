{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.Config where

import Control.Lens
import Data.Text (Text)

data MoeConfig = MoeConfig
  {
    _server :: Text
  , _serverPort :: Int
  , _localPort :: Int
  , _password :: Text
  , _method :: Text
  }
  deriving (Show, Eq)

makeLenses ''MoeConfig

defaultMoeConfig :: MoeConfig
defaultMoeConfig = MoeConfig
  {
    _server = "localhost"
  , _serverPort = 1190
  , _localPort = 1090
  , _password = "moesocks"
  , _method = "none"
  }


