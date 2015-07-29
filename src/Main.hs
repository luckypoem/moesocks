module Main where

import Network.MoeSocks.App
import Network.MoeSocks.Options
import Options.Applicative hiding (Parser)

main :: IO ()
main = execParser opts >>= moeApp

