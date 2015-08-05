module Main where

import Control.Monad.Except
import Control.Monad.Reader hiding (local)
import Network.MoeSocks.App
import Network.MoeSocks.Config
import Network.MoeSocks.Helper
import Network.MoeSocks.Options
import Network.MoeSocks.Type
import Options.Applicative hiding (Parser)
import Prelude hiding ((-))
import System.Exit

main :: IO ()
main = do
  _options <- execParser opts
  r <- runExceptT - runReaderT moeApp -
                      Env _options defaultMoeConfig
  case r of
    Left e -> pute e >> exitFailure
    Right _ -> pure ()

