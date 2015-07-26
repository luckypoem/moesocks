{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main where

import "cipher-aes" Crypto.Cipher.AES
import Air.Env hiding ((.), has, take, puts) 
import Control.Lens
import Data.ByteString.Lens
import Data.Maybe
import Network.MoeSocks.App
import Network.MoeSocks.Config
import Network.MoeSocks.OptionParser
import Network.MoeSocks.Type
import Prelude ((.))
import System.Random
import qualified Prelude as P

main :: IO ()
main = moeApp - 
  defaultMoeOptions &
    configFile .~ "config.json"

