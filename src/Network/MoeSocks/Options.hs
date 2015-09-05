{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.Options where

import Control.Lens
import Data.Aeson
import Data.IP
import Data.Maybe
import Data.Text (Text)
import Data.Text.Lens
import Network.MoeSocks.Config
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
import Options.Applicative hiding (Parser)
import Prelude hiding ((-), takeWhile)
import System.Log.Logger
import qualified Data.Text as T
import qualified Options.Applicative as O
import Data.Attoparsec.Text (Parser, takeWhile, char, decimal, skipSpace, 
                              parseOnly, many', choice)

textOption :: O.Mod O.OptionFields String -> O.Parser Text
textOption x = strOption x <&> view (from _Text)

defaultHelp :: Text -> Text -> Mod f a
defaultHelp val x = help - x <> ", default: " <> val & view _Text

textParam :: O.Mod O.OptionFields String -> O.Parser (Maybe Value)
textParam = optional . fmap toJSON . textOption

intParam :: O.Mod O.OptionFields Int -> O.Parser (Maybe Value)
intParam = optional . fmap toJSON . option auto

bool_To_Maybe :: Bool -> Maybe Bool
bool_To_Maybe False = Nothing
bool_To_Maybe True = Just True

boolParam :: O.Mod O.FlagFields Bool -> O.Parser (Maybe Value)
boolParam = (fmap . fmap) toJSON . fmap bool_To_Maybe . switch

optionParser :: O.Parser Options
optionParser = 
  let _c = defaultConfig
      _mode = ( textOption -
                    short 'r'
                <>  long "role"
                <>  metavar "ROLE"
                <>  defaultHelp "local" 
                                "Tell moesocks to run as local or remote"
              ) <|> pure "local" 
  
      parseMode :: Text -> RunningMode
      parseMode x 
        | x `elem` ["server", "remote"] = RemoteMode
        | x `elem` ["client", "local"] = LocalMode
        | x == "debug" = DebugMode
        | otherwise = DebugMode

      _disable_SOCKS5 :: O.Parser Bool
      _disable_SOCKS5 = switch -
                      long "disable-socks5"
                  <>  help ("Do not start a SOCKS5 server on local. It can be "
                          <> "useful to run moesocks only as a secure tunnel")
      
      _listMethods :: O.Parser Bool
      _listMethods = switch -
                      long "list-methods"
                  <>  help "Show supported encryption methods"
                  
      _tcpBufferSize = intParam -
                              long "tcp-buffer-size"
                          <>  metavar "SIZE"
                          <>  defaultHelp (_c ^. tcpBufferSize
                                            & show
                                            & view (from _Text))
                                          ("The number of packets used as a "
                                            <> "buffer. A packet can hold "
                                            <> "at most 4K of data")

      _config = optional - textOption -
                      short 'c'
                  <>  long "config"
                  <>  metavar "CONFIG"
                  <>  help "Set the path of the configuration file"
                 
      _remote = textParam -
                      short 's'
                  <>  metavar "REMOTE"
                  <>  defaultHelp (_c ^. remoteHost)
                                  "remote address"
  
      _remotePort = intParam -
                          short 'p'
                      <>  metavar "REMOTE_PORT"
                      <>  defaultHelp (_c ^. remotePort 
                                          & show 
                                          & view (from _Text))
                                      "remote port"

      _local = textParam -
                      short 'b'
                  <>  metavar "LOCAL"
                  <>  defaultHelp (_c ^. localHost)
                                  "local address"

      _localPort = intParam -
                        short 'l'
                    <>  metavar "LOCAL PORT"
                    <>  defaultHelp (_c ^. localPort
                                        & show
                                        & view (from _Text))
                                    "local port"

      _password = textParam -
                        short 'k'
                    <>  metavar "PASSWORD"
                    <>  help "password"

      _method   = textParam -
                        short 'm'
                    <>  metavar "METHOD"
                    <> defaultHelp (_c ^. method)
                                    "encryption method"

      _timeout  = intParam - 
                        short 't'
                    <>  metavar "TIMEOUT"
                    <>  defaultHelp (_c ^. timeout
                                        & show
                                        & view (from _Text))
                                    "Timeout connection in seconds"
                        
      _fastOpen = boolParam -
                        long "fast-open"
                    <>  help ("Use TCP_FASTOPEN, requires Linux 3.7+")
      
      _obfuscation :: O.Parser Bool 
      _obfuscation = switch -
                          short 'o'
                      <>  long "obfuscation"
                      <>  help ("Turn on simple obfuscation while still "
                               <> "being compatible with "
                               <> "shadowsocks protocol, at the cost of "
                               <> "about 10-20% performance degradation.")


      _verbosity :: O.Parser Priority 
      _verbosity = flag INFO DEBUG -
                          short 'v'
                      <>  long "verbose"
                      <>  help "Turn on logging"

      _forwardTCP :: O.Parser (Maybe Text)
      _forwardTCP = optional - textOption -
                          short 'T'
                      <>  long "tcp"
                      <>  metavar "port:host:hostport"
                      <>  help (""
                                <> "Specify that the given TCP port on the "
                                <> "local"
                                <> "(client) host is to be forwarded to the "
                                <> "given host and port on the remote side."
                                )
  


      _forwardUDP :: O.Parser (Maybe Text)
      _forwardUDP = optional - textOption -
                          short 'U'
                      <>  long "udp"
                      <>  metavar "port:host:hostport"
                      <>  help (""
                                <> "Specify that the given UDP port on the "
                                <> "local"
                                <> "(client) host is to be forwarded to the "
                                <> "given host and port on the remote side."
                                )
  
      forwardParser ::  Parser Forward
      forwardParser = do
        skipSpace
        _forwardPort <- decimal
        char ':'
        _forwardRemoteHost <- 
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
        _forwardRemotePort <- decimal

        pure - Forward  
                        _forwardPort
                        _forwardRemoteHost
                        _forwardRemotePort

      forwardListParser :: Parser [Forward]
      forwardListParser = many' forwardParser

      parseForwarding :: Maybe Text -> [Forward]
      parseForwarding x = x ^. _Just
          & parseOnly forwardListParser 
          & toListOf (traverse . traverse)

      _forbidden_IPs :: O.Parser (Maybe Text)
      _forbidden_IPs = optional - textOption -
                          long "forbidden-ip"
                      <>  metavar "IPLIST"
                      <>  defaultHelp (defaultOptions ^. forbidden_IPs
                                        & map show
                                        & map (view - from _Text)
                                        & T.intercalate ", ")
                                (""
                                <> "comma seperated IP list forbidden to "
                                <> "connect"
                                )
     
      parseForbidden_IP :: Maybe Text -> [IPRange]
      parseForbidden_IP = maybe (defaultOptions ^. forbidden_IPs) -
                                (toListOf - each 
                                          . to T.strip 
                                          . _Text 
                                          . _Show
                                ) . T.splitOn ","


      tag :: a -> O.Parser (Maybe b) -> O.Parser (Maybe (a, b))
      tag x = fmap . fmap - ((,) x)


      _params :: O.Parser [(Text, Value)]
      _params = 
        [ tag "_remoteHost"         _remote    
        , tag "_remotePort"     _remotePort
        , tag "_localHost"          _local     
        , tag "_localPort"      _localPort 
        , tag "_password"       _password
        , tag "_method"         _method 
        , tag "_timeout"        _timeout
        , tag "_tcpBufferSize"  _tcpBufferSize
        , tag "_fastOpen"       _fastOpen
        ]
        & sequenceA
        & fmap catMaybes
          
  in
        

  Options 
              <$> fmap parseMode _mode 
              <*> _config
              <*> _verbosity
              <*> fmap parseForwarding _forwardTCP
              <*> fmap parseForwarding _forwardUDP
              <*> _disable_SOCKS5
              <*> _obfuscation
              <*> fmap parseForbidden_IP _forbidden_IPs
              <*> _listMethods
              <*> _params

opts :: ParserInfo Options
opts = info (helper <*> optionParser) - 
        fullDesc
    <>  progDesc "A SOCKS5 proxy using the client / server architecture"
    <>  header "A functional firewall killer"

