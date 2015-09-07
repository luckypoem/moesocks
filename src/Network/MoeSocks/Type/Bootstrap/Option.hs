{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.MoeSocks.Type.Bootstrap.Option where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Lens
import Data.IP
import Data.Word
import GHC.Generics (Generic)
import System.Log.Logger
import Numeric (showHex)
import qualified Data.Strict as S
import qualified Data.List as L

data RunningMode = RemoteMode | LocalMode | DebugMode
      deriving (Show, Eq)

type Port = Int

data Forward = Forward
  {
    _forwardLocalPort :: Port
  , _forwardTargetHost :: Text
  , _forwardTargetPort :: Port
  }
  deriving (Show, Eq)


makeLenses ''Forward

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
