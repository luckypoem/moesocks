
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.MoeSocks.Type.Common where

import Control.Lens
import Data.Text (Text)

type Port = Int

data Forward = Forward
  {
    _forwardLocalPort :: Port
  , _forwardTargetHost :: Text
  , _forwardTargetPort :: Port
  }
  deriving (Show, Eq)

makeLenses ''Forward
