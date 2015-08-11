{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.Options where

import Control.Lens
import Data.Text.Lens
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
import Options.Applicative hiding (Parser)
import Prelude hiding ((-), takeWhile)
import System.Log.Logger
import qualified Options.Applicative as O
import Data.Attoparsec.Text (Parser, takeWhile, char, decimal, skipSpace, 
                              parseOnly, many', choice)

optionParser :: O.Parser MoeOptions
optionParser = 
  let _mode = ( strOption -
                    short 'm'
                <>  long "mode"
                <>  metavar "MODE"
                <>  help "Tell moesocks to runs in local or remote mode"
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
                  <>  help "Point to the path of the configuration file"
                ) <|> pure "config.json"
                 
  in

  let __verbosity :: O.Parser Priority 
      __verbosity = flag INFO DEBUG -
                          short 'v'
                      <>  long "verbose"
                      <>  help "Turn on logging"
  in


  let _forwarding :: O.Parser String
      _forwarding = ( strOption -
                          short 'L'
                      <>  metavar "port:host:hostport"
                      <>  help (""
                                <> "Specify that the given port on the local"
                                <> "(client) host is to be forwarded to the "
                                <> "given host and port on the remote side."
                                )
                    ) <|> pure ""
                      

      forwardingParser :: Parser LocalForwarding
      forwardingParser = do
        skipSpace
        _localForwardingPort <- decimal
        char ':'
        _localForwardingRemoteHost <- 
          choice
            [
              do 
                char '['
                _h <- takeWhile (/= ']')
                char ']'
                return _h
            , takeWhile (/= ':')
            ]
        char ':'
        _localForwardingRemotePort <- decimal

        pure - LocalForwarding  _localForwardingPort
                                _localForwardingRemoteHost
                                _localForwardingRemotePort

      forwardingListParser :: Parser [LocalForwarding]
      forwardingListParser = many' forwardingParser

      parseForwarding :: String -> [LocalForwarding]
      parseForwarding x = 
        x ^. from _Text 
          & parseOnly forwardingListParser 
          & toListOf (traverse . traverse)
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


