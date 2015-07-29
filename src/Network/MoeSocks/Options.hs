{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.Options where

import Control.Lens
import Data.Text.Lens
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
import Options.Applicative hiding (Parser)
import Prelude hiding ((-))
import qualified Options.Applicative as O


optionParser :: O.Parser MoeOptions
optionParser = 
  let _mode = strOption -
                      short 'm'
                  <>  long "mode"
                  <>  metavar "MODE"
                  <>  help "local | remote"
  in
  let _config = strOption -
                      short 'c'
                  <>  long "config"
                  <>  metavar "CONFIG"
                  <>  help "path to the configuration file"
  in
{-data Verbosity = Normal | Verbose-}

{-flag Normal Verbose-}
  {-( long "verbose"-}
 {-<> short 'v'-}
 {-<> help "Enable verbose mode" )-}

  let _polipo = flag Strict Polipo -
                      long "polipo"
                  <>  help "run as a polipo compatible socks5 server"
  in

  let parseMode :: String -> RunningMode
      parseMode x 
        | x `elem` ["server", "remote"] = RemoteMode
        | x `elem` ["client", "local"] = LocalMode
        | x == "debug" = DebugMode
        | otherwise = DebugMode
  in

  MoeOptions 
              <$> fmap parseMode _mode 
              <*> fmap (view packed) _config
              <*> _polipo

opts :: ParserInfo MoeOptions
opts = info (helper <*> optionParser) - 
        fullDesc
    <>  progDesc "A socks5 proxy using the client / server architecture"
    <>  header "moesocks - moe for all"


