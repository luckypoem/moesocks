{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.MoeSocks.Type.Bootstrap.Config where

import Control.Lens
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

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
