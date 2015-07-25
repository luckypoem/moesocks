{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Control.Lens
import Prelude ((.))
import Air.Env hiding ((.), has, take, puts) 

import Network.MoeSocks.Type
import Network.MoeSocks.App
import Network.MoeSocks.Config

import Data.ByteString.Lens
import System.Random
import qualified Prelude as P
import "cipher-aes" Crypto.Cipher.AES
import Data.Maybe

main :: IO ()
main = moeApp - defaultMoeOptions

