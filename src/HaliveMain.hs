{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader hiding (local)
import Data.ByteString.Lens
import Data.Maybe
import Network.MoeSocks.App
import Network.MoeSocks.Default
import Network.MoeSocks.Helper
import Network.MoeSocks.Options
import Network.MoeSocks.Type
import qualified Network.MoeSocks.Type.Bootstrap.Option as O
import Network.MoeSocks.Type.Bootstrap.Config
import Prelude hiding ((-))
import System.Exit
import System.Log.Logger
import System.Random

main :: IO ()
main = do
  let _options = defaultOptions 
                                    {-& configFile .~ Just "halive.json"-}
                                    & O.configFile .~ Just "config.json"
                                    & O.forward_UDPs .~ 
                                                [Forward 5301 "localhost" 53]
                                    {-& verbosity .~ INFO-}
                                    {-& verbosity .~ DEBUG-}
                                    & O.obfuscation .~ True
                                    & O.forbidden_IPs .~ []
                                    {-& listMethods .~ True-}

  withGateOptions _options - do
    r <- runExceptT - runReaderT moeApp _options
    case r of
      Left e -> error_ e >> exitFailure
      Right _ -> pure ()

