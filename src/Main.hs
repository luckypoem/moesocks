module Main where

import Control.Monad.Except
import Network.MoeSocks.App
import Network.MoeSocks.Bootstrap
import Network.MoeSocks.Helper
import Network.MoeSocks.Options
import Options.Applicative hiding (Parser)
import Prelude hiding ((-))
import System.Exit

main :: IO ()
main = do
  _options <- execParser opts
  
  withGateOptions _options - do
    r <- runExceptT - moeApp _options
    case r of
      Left e -> error_ e >> exitFailure
      Right _ -> pure ()

