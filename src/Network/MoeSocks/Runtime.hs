{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.MoeSocks.Runtime where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Writer hiding (listen)
import Data.IP
{-import Data.List (intercalate)-}
{-import Data.Text (Text)-}
import Data.Text.IO as T
import Data.Text.Lens
import Network.MoeSocks.Bootstrap
import Network.MoeSocks.Default
import Network.MoeSocks.Encrypt
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
import Prelude hiding ((-), take)
import System.Log.Formatter
import System.Log.Handler.Simple
import System.Log.Logger
import qualified Data.Text as T
import qualified Network.MoeSocks.Type.Bootstrap.Config as C
import qualified Network.MoeSocks.Type.Bootstrap.Option as O
import qualified System.IO as IO
import qualified System.Log.Handler as LogHandler

initLogger :: Priority -> IO ()
initLogger aLevel = do
  stdoutHandler <- streamHandler IO.stdout DEBUG
  let formattedHandler = 
          LogHandler.setFormatter stdoutHandler -
            --"[$time : $loggername : $prio]
            simpleLogFormatter "$time $prio\t $msg"

  updateGlobalLogger rootLoggerName removeHandler

  updateGlobalLogger "moe" removeHandler
  updateGlobalLogger "moe" - addHandler formattedHandler
  updateGlobalLogger "moe" - setLevel aLevel



loadJobs :: C.Config -> O.Options -> [Job]
loadJobs aConfig someOptions = 
  let _c = aConfig

      _remote_TCP_Relay =   
          RemoteRelay
            Remote_TCP_Relay
            (_c ^. C.remoteHost)
            (_c ^. C.remotePort)

      _remote_UDP_Relay = 
          RemoteRelay
            Remote_UDP_Relay
            (_c ^. C.remoteHost)
            (_c ^. C.remotePort)

      _localService :: LocalServiceType -> LocalService
      _localService = LocalService 
                        (_c ^. C.localHost)
                        (_c ^. C.remoteHost)
                        (_c ^. C.remotePort)

      _localService_TCP_Forwards = 
          someOptions ^. O.forward_TCPs 
            & map (_localService . LocalService_TCP_Forward)

      _localService_UDP_Forwards =
          someOptions ^. O.forward_UDPs 
            & map (_localService . LocalService_UDP_Forward)

      _localService_SOCKS5 =
          LocalService_SOCKS5 (_c ^. C.localPort)
            & _localService

      _remoteRelays = [_remote_TCP_Relay, _remote_UDP_Relay]
      _localServices = 
        let
          _localService_SOCKS5s = 
            if someOptions ^. O.disable_SOCKS5
              then []
              else pure _localService_SOCKS5
        in

        _localService_TCP_Forwards
        <> _localService_UDP_Forwards
        <> _localService_SOCKS5s
  in

  map RemoteRelayJob _remoteRelays
  <> map LocalServiceJob _localServices

filterJobs :: O.RunningMode -> [Job] -> [Job]
filterJobs = \case
  O.DebugMode -> id
  O.RemoteMode -> filter - is _RemoteRelayJob
  O.LocalMode -> filter - is _LocalServiceJob


initEnv :: C.Config -> O.Options -> ExceptT String IO Env
initEnv aConfig someOptions = do
  let _o = someOptions
      _c = aConfig

  io - initLogger - _o ^. O.verbosity
  io - debug_ - show _o
  
  _config <- loadConfig - _o

  let _method = _c ^. C.method

  _cipherBox <- (io - initCipherBox _method (_c ^. C.password)) >>= \case
    Nothing -> throwError - "Invalid method '"
                            <> _method ^. _Text
    Just (a, b, c, d) -> pure - CipherBox a b c d

  let _readDenyList :: IO [IPRange]
      _readDenyList = 
        case someOptions ^. O.denyList of
          Nothing -> pure []
          Just _denyListPath ->
            T.readFile (_denyListPath ^. _Text)
              <&> T.lines 
              <&> parseIPRangeList
                

  _denyList <- io - _readDenyList

  io - debug_ - "denyList: " <> show (length _denyList) <> " items"

  let _env = defaultEnv
              & cipherBox .~ _cipherBox
              & denyList .~ _denyList

  pure - _env

initRuntime :: C.Config -> O.Options -> ExceptT String IO (Runtime)
initRuntime aConfig someOptions = do
  let _c = aConfig
      _o = someOptions
      _s = _c ^. C.socketOption_TCP_NOTSENT_LOWAT

  _env <- initEnv aConfig someOptions
  
  let _env' = _env
        & timeout                        .~ _c ^. C.timeout
        & tcpBufferSize                  .~ _c ^. C.tcpBufferSize
        & throttle                       .~ _c ^. C.throttle
        & throttleSpeed                  .~ _c ^. C.throttleSpeed
        & obfuscationFlushBound          .~ _c ^. C.obfuscationFlushBound 
        & fastOpen                       .~ _c ^. C.fastOpen
        & socketOption_TCP_NOTSENT_LOWAT .~ _s
        & obfuscation                    .~ _o ^. O.obfuscation
        & forbidden_IPs                  .~ (_c ^. C.forbidden_IPs
                                                & parseIPRangeList)
                                            
                                        
  
  let _jobs = loadJobs _c _o & filterJobs (_o ^. O.runningMode)

  pure -  ( defaultRuntime
              & env .~ _env'
              & jobs .~ _jobs
          )
