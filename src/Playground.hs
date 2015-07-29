module Main where

import Prelude ()
import Air.Env

import Control.Lens

import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

main = do
  debugM "moe" "Hello!"
  updateGlobalLogger "moe" - setLevel DEBUG
