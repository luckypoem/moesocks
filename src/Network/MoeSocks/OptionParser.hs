{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PackageImports #-}

module Network.MoeSocks.OptionParser where

import Control.Lens
import Prelude ((.))
import Air.Env hiding ((.), has, take, puts) 

import Network.MoeSocks.Type
import Network.MoeSocks.App

import Data.ByteString.Lens
import System.Random
import qualified Prelude as P
import "cipher-aes" Crypto.Cipher.AES
import Data.Maybe
import Data.Text.Lens

import Options.Applicative hiding (Parser)
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

  let parseMode :: String -> RunningMode
      parseMode x 
        | x `elem` ["server", "remote"] = RemoteMode
        | x `elem` ["client", "local"] = LocalMode
        | x == "debug" = DebugMode
  in

  MoeOptions <$> fmap parseMode _mode <*> fmap (view packed) _config

opts :: ParserInfo MoeOptions
opts = info (helper <*> optionParser) - 
        fullDesc
    <>  progDesc "A socks5 proxy using the client / server architecture"
    <>  header "moesocks - moe for all"


