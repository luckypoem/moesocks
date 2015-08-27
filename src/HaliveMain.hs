{-# LANGUAGE OverloadedStrings #-}

module Main where

import Air.Env hiding ((.), has, take, puts, (-)) 
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader hiding (local)
import Data.ByteString.Lens
import Data.Maybe
import Network.MoeSocks.App
import Network.MoeSocks.Config
import Network.MoeSocks.Helper
import Network.MoeSocks.Options
import Network.MoeSocks.Type
import Prelude ((.))
import System.Exit
import System.Log.Logger
import System.Random
import qualified Prelude as P

main :: IO ()
main = do
  let _options = defaultMoeOptions 
                                    & configFile .~ Just "config.json"
                                    & forward_UDP .~ 
                                                [Forward 5301 "localhost" 53]
                                    & verbosity .~ INFO
                                    & obfuscation .~ True
                                    & forbidden_IP .~ []

  r <- runExceptT - runReaderT moeApp _options
  case r of
    Left e -> pute e >> exitFailure
    Right _ -> pure ()

