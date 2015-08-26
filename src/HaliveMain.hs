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
import Network.MoeSocks.Type
import Network.MoeSocks.Options
import Prelude ((.))
import System.Exit
import System.Random
import qualified Prelude as P
import System.Log.Logger

main :: IO ()
main = do
  let _options = defaultMoeOptions 
                                    & configFile .~ Just "config.json"
                                    & forwardUDP .~ 
                                                [Forward 5301 "localhost" 53]
                                    {-& verbosity .~ INFO-}
                                    & obfuscation .~ True
                                    & forbidden_IP .~ []

  r <- runExceptT - runReaderT moeApp _options
  case r of
    Left e -> pute e >> exitFailure
    Right _ -> pure ()

