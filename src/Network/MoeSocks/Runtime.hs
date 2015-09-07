{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.MoeSocks.Runtime where

import Control.Lens
import Control.Monad.Writer hiding (listen)
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
import Network.MoeSocks.Default
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


initRuntime :: C.Config -> O.Options -> Runtime
initRuntime aConfig someOptions = 
  let _c = aConfig
      _o = someOptions
  in
  defaultRuntime
    & timeout                        .~ _c ^. C.timeout
    & tcpBufferSize                  .~ _c ^. C.tcpBufferSize
    & throttle                       .~ _c ^. C.throttle
    & throttleSpeed                  .~ _c ^. C.throttleSpeed
    & obfuscationFlushBound          .~ _c ^. C.obfuscationFlushBound 
    & fastOpen                       .~ _c ^. C.fastOpen
    & socketOption_TCP_NOTSENT_LOWAT .~ _c ^. C.socketOption_TCP_NOTSENT_LOWAT
    & obfuscation                    .~ _o ^. O.obfuscation
    & forbidden_IPs                  .~ _o ^. O.forbidden_IPs
