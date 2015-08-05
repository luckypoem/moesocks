{-# LANGUAGE OverloadedStrings #-}

module Main where

import Air.Env hiding ((.), has, take, puts, (-)) 
import Control.Lens
import Data.ByteString.Lens
import Data.Maybe
import Network.MoeSocks.App
import Network.MoeSocks.Config
import Network.MoeSocks.Type
import Prelude ((.))
import System.Random
import qualified Prelude as P
import Control.Monad.Reader hiding (local)
import Control.Monad.Except
import Network.MoeSocks.Helper
import System.Exit

main :: IO ()
main = do
  let _options = defaultMoeOptions & configFile .~ "config.json"
  r <- runExceptT - runReaderT moeApp -
                      Env _options defaultMoeConfig
  case r of
    Left e -> pute e >> exitFailure
    Right _ -> pure ()

