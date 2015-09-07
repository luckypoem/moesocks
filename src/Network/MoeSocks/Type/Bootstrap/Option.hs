{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.MoeSocks.Type.Bootstrap.Option where

import Control.Lens
import Data.Aeson
import Data.Text (Text)
import Data.IP
import System.Log.Logger
import Network.MoeSocks.Type.Common

data RunningMode = RemoteMode | LocalMode | DebugMode
      deriving (Show, Eq)

data Options = Options
  {
    _runningMode :: RunningMode
  , _configFile :: Maybe Text
  , _verbosity :: Priority
  , _forward_TCPs :: [Forward]
  , _forward_UDPs :: [Forward]
  , _disable_SOCKS5 :: Bool
  , _obfuscation :: Bool
  , _forbidden_IPs :: [IPRange]
  , _listMethods :: Bool
  , _params :: [(Text, Value)]
  }
  deriving (Show, Eq)

makeLenses ''Options
