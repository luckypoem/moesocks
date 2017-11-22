{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.MoeSocks.Type.Bootstrap.Option where

import Control.Lens
import Data.Aeson (Value)
import Data.Text (Text)
import Network.MoeSocks.Type.Common (Forward)
import System.Log.Logger (Priority)

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
  , _listMethods :: Bool
  , _showDefaultConfig :: Bool
  , _params :: [(Text, Value)]
  , _denyList :: Maybe Text
  }
  deriving (Show, Eq)

makeLenses ''Options
