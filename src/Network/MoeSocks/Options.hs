{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.Options where

import Control.Lens
import Data.Text.Lens
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
import Network.MoeSocks.Config
import Options.Applicative hiding (Parser)
import Prelude hiding ((-))
import System.Log.Logger
import qualified Options.Applicative as O

optionParser :: O.Parser MoeOptions
optionParser = 
  let _mode = ( strOption -
                    short 'm'
                <>  long "mode"
                <>  metavar "MODE"
                <>  help "local | remote"
              ) <|> pure "local" 
  
      parseMode :: String -> RunningMode
      parseMode x 
        | x `elem` ["server", "remote"] = RemoteMode
        | x `elem` ["client", "local"] = LocalMode
        | x == "debug" = DebugMode
        | otherwise = DebugMode
  in


  let _config = ( strOption -
                      short 'c'
                  <>  long "config"
                  <>  metavar "CONFIG"
                  <>  help "path to the configuration file"
                ) <|> pure "config.json"
                 
  in

  let __verbosity :: O.Parser Priority 
      __verbosity = ( option auto - 
                          short 'v'
                      <>  long "verbose"
                      <>  metavar "PRIORITY"
                      <>  help "DEBUG|INFO|NOTICE"
                    ) <|> pure INFO 
  in


  let _forwarding :: O.Parser String
      _forwarding = ( strOption -
                          short 'L'
                      <>  metavar "port:host:hostport"
                      <>  help ("as in ssh: " 
                                <> "Specifies that the given port on the local"
                                <> "(client) host is to be forwarded to the "
                                <> "given host and port on the remote side."
                                )
                    ) <|> pure ""
                      

      parseForwarding :: String -> [LocalForwarding]
      parseForwarding _ = defaultMoeOptions ^. localForwarding
  in
        

  MoeOptions 
              <$> fmap parseMode _mode 
              <*> fmap (view packed) _config
              <*> __verbosity
              <*> fmap parseForwarding _forwarding

opts :: ParserInfo MoeOptions
opts = info (helper <*> optionParser) - 
        fullDesc
    <>  progDesc "A socks5 proxy using the client / server architecture"
    <>  header "moesocks - moe for all"


