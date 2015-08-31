{-# LANGUAGE OverloadedStrings #-}

module Main where

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
import Prelude hiding ((-))
import System.Exit
import System.Log.Logger
import System.Random

main :: IO ()
main = do
  let _options = defaultMoeOptions 
                                    & configFile .~ Just "config.json"
                                    & forward_UDP .~ 
                                                [Forward 5301 "localhost" 53]
                                    {-& verbosity .~ INFO-}
                                    & verbosity .~ DEBUG
                                    & obfuscation .~ True
                                    & forbidden_IP .~ []
                                    {-& listMethods .~ True-}

  withGateOptions _options - do
    r <- runExceptT - runReaderT moeApp _options
    case r of
      Left e -> error_ e >> exitFailure
      Right _ -> pure ()

