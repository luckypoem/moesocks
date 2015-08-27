module Main where

import Control.Monad.Except
import Control.Monad.Reader hiding (local)
import Network.MoeSocks.App
import Network.MoeSocks.Helper
import Network.MoeSocks.Options
import Options.Applicative hiding (Parser)
import Prelude hiding ((-))
import System.Exit

main :: IO ()
main = do
  _options <- execParser opts
  r <- runExceptT - runReaderT moeApp _options
  case r of
    Left e -> error_ e >> exitFailure
    Right _ -> pure ()

