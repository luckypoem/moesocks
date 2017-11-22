{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.Options where

import           Control.Lens
import           Data.Aeson (Value, toJSON)
import           Data.Attoparsec.Text (Parser, takeWhile, char, decimal)
import           Data.Attoparsec.Text (skipSpace, parseOnly, many', choice)
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lens
import qualified Options.Applicative as O
import           Options.Applicative hiding (Parser)
import           System.Log.Logger (Priority(DEBUG, INFO))

import           Network.MoeSocks.Default (defaultConfig)
import qualified Network.MoeSocks.Type.Bootstrap.Config as C
import           Network.MoeSocks.Type.Bootstrap.Option (Options(..), RunningMode(..))
import           Network.MoeSocks.Type.Common (Forward(Forward))

import           Network.MoeSocks.Helper ((-))
import           Prelude hiding ((-), takeWhile)

textOption :: O.Mod O.OptionFields String -> O.Parser Text
textOption x = strOption x <&> review _Text

defaultHelp :: Text -> Text -> Mod f a
defaultHelp val x = help - x <> ", default: " <> val & view _Text

textParam :: O.Mod O.OptionFields String -> O.Parser (Maybe Value)
textParam = optional . fmap toJSON . textOption

commaSeperatedArrayParam :: O.Mod O.OptionFields String
                          -> O.Parser (Maybe Value)
commaSeperatedArrayParam =
  optional . fmap toJSON . fmap (filter (isn't _Empty)
                                    . map T.strip
                                    . T.splitOn ",")
                          . textOption

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

      _showDefaultConfig :: O.Parser Bool
      _showDefaultConfig = switch -
                      long "show-default-config"
                  <>  help "Show default json configuration"

      _tcpBufferSize = intParam -
                              long "tcp-buffer-size"
                          <>  metavar "SIZE"
                          <>  defaultHelp (_c ^. C.tcpBufferSize
                                            & show
                                            & review _Text)
                                          ("The number of packets used as a "
                                            <> "buffer. A packet can hold "
                                            <> "at most 4K of data")

      _config = optional - textOption -
                      short 'c'
                  <>  long "config"
                  <>  metavar "CONFIG"
                  <>  help "Set the path of the configuration file"

      _denyList = optional - textOption -
                        long "deny-list"
                    <>  metavar "ACL"
                    <>  help "Block IPs from an access control list file."


      _remoteHost = textParam -
                      short 's'
                  <>  metavar "REMOTE"
                  <>  defaultHelp (_c ^. C.remoteHost)
                                  "remote address"

      _remotePort = intParam -
                          short 'p'
                      <>  metavar "REMOTE_PORT"
                      <>  defaultHelp (_c ^. C.remotePort
                                          & show
                                          & review _Text)
                                      "remote port"

      _localHost = textParam -
                      short 'b'
                  <>  metavar "LOCAL"
                  <>  defaultHelp (_c ^. C.localHost)
                                  "local address"

      _localPort = intParam -
                        short 'l'
                    <>  metavar "LOCAL PORT"
                    <>  defaultHelp (_c ^. C.localPort
                                        & show
                                        & review _Text)
                                    "local port"

      _password = textParam -
                        short 'k'
                    <>  metavar "PASSWORD"
                    <>  help "password"

      _method   = textParam -
                        short 'm'
                    <>  metavar "METHOD"
                    <> defaultHelp (_c ^. C.method)
                                    "encryption method"

      _timeout  = intParam -
                        short 't'
                    <>  metavar "TIMEOUT"
                    <>  defaultHelp (_c ^. C.timeout
                                        & show
                                        & review _Text)
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

      _forbidden_IPs = commaSeperatedArrayParam -
                          long "forbidden-ip"
                      <>  metavar "IPLIST"
                      <>  defaultHelp (defaultConfig ^. C.forbidden_IPs
                                        & T.intercalate ", ")
                                (""
                                <> "comma seperated IP list forbidden to "
                                <> "connect"
                                )

      tag :: a -> O.Parser (Maybe b) -> O.Parser (Maybe (a, b))
      tag x = fmap . fmap - ((,) x)


      _params :: O.Parser [(Text, Value)]
      _params =
        [ tag "_remoteHost"     _remoteHost
        , tag "_remotePort"     _remotePort
        , tag "_localHost"      _localHost
        , tag "_localPort"      _localPort
        , tag "_password"       _password
        , tag "_method"         _method
        , tag "_timeout"        _timeout
        , tag "_tcpBufferSize"  _tcpBufferSize
        , tag "_fastOpen"       _fastOpen
        , tag "_forbidden_IPs"  _forbidden_IPs
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
              <*> _listMethods
              <*> _showDefaultConfig
              <*> _params
              <*> _denyList

opts :: ParserInfo Options
opts = info (helper <*> optionParser) -
        fullDesc
    <>  progDesc "A SOCKS5 proxy using the client / server architecture"
    <>  header "A functional firewall killer"
