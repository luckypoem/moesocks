{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.MoeSocks.Runtime where

import Control.Lens
import Control.Monad.Writer hiding (listen)
import Control.Monad
import Control.Monad.Except
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
import Network.MoeSocks.Modules.Resource
import Network.MoeSocks.Encrypt
import Network.MoeSocks.Default
import Data.Text.Lens
import qualified Network.MoeSocks.Type.Bootstrap.Config as C
import qualified Network.MoeSocks.Type.Bootstrap.Option as O
import Prelude hiding ((-), take)
import System.Log.Formatter
import System.Log.Handler.Simple
import System.Log.Logger
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
      _localServices = _localService_TCP_Forwards
                    <> _localService_UDP_Forwards
                    <> pure _localService_SOCKS5
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

  let _env = defaultEnv
              & options   .~ _o
              & config    .~ _c
              & cipherBox .~ _cipherBox

  pure - _env

initRuntime :: C.Config -> O.Options -> ExceptT String IO (Runtime, [Job])
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
        & forbidden_IPs                  .~ _o ^. O.forbidden_IPs
  
  let _jobs = loadJobs _c _o & filterJobs (_o ^. O.runningMode)

  pure -  ( defaultRuntime
              & env .~ _env'
          , _jobs
          )
