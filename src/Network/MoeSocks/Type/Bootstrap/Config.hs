{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.MoeSocks.Type.Bootstrap.Config where

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

data Config = Config
  {
    _remoteHost :: Text
  , _remotePort :: Int
  , _localHost :: Text
  , _localPort :: Int
  , _password :: Text
  , _method :: Text
  , _timeout :: Int
  , _tcpBufferSize :: Int -- in packets
  , _throttle :: Bool
  , _throttleSpeed :: Double
  , _obfuscationFlushBound :: Int -- should be greater then MTU
  , _fastOpen :: Bool
  , _socketOption_TCP_NOTSENT_LOWAT :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON Config
instance ToJSON Config

makeLenses ''Config
