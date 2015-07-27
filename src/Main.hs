module Main where

import Network.MoeSocks.App
import Network.MoeSocks.OptionParser
import Options.Applicative hiding (Parser)

main :: IO ()
main = execParser opts >>= moeApp

