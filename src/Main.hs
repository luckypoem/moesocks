{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PackageImports #-}

module Main where

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

import Options.Applicative hiding (Parser)
import qualified Options.Applicative as O

optionsParser :: O.Parser MoeOptions
optionsParser = 
  let _mode = strOption -
                    short 'm'
                 <> metavar "MODE"
                 <> help "local | remote"
  in
  let _config = strOption -
                    short 'c'
                 <> metavar "CONFIG"
                 <> help "path to the configuration file"

  in

  let parseMode :: String -> RunningMode
      parseMode x 
        | x `elem` ["server", "remote"] = ServerMode
        | x `elem` ["client", "local"] = ClientMode
        | x == "debug" = DebugMode
  in

  MoeOptions <$> fmap parseMode _mode <*> _config

opts :: ParserInfo MoeOptions
opts = info (helper <*> optionsParser) - 
        fullDesc
    <>  progDesc "A socks5 proxy with client / server architecture"
    <>  header "moesocks - moe for all"

main :: IO ()
main = execParser opts >>= moeApp

