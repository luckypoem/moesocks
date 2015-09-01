{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.MoeSocks.Runtime where

import Control.Lens
import Control.Monad.Writer hiding (listen)
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
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


config_To_Jobs :: Config -> Options -> [Job]
config_To_Jobs aConfig aOptions = 
  let _c = aConfig

      _remote_TCP_Relay =   
          RemoteRelay
            Remote_TCP_Relay
            (_c ^. remoteAddress)
            (_c ^. remotePort)

      _remote_UDP_Relay = 
          RemoteRelay
            Remote_UDP_Relay
            (_c ^. remoteAddress)
            (_c ^. remotePort)

      _localService :: LocalServiceType -> LocalService
      _localService = LocalService 
                        (_c ^. localAddress)
                        (_c ^. remoteAddress)
                        (_c ^. remotePort)

      _localService_TCP_Forwards = 
          aOptions ^. forward_TCPs 
            & map (_localService . LocalService_TCP_Forward)

      _localService_UDP_Forwards =
          aOptions ^. forward_UDPs 
            & map (_localService . LocalService_UDP_Forward)

      _localService_SOCKS5 =
          LocalService_SOCKS5 (_c ^. localPort)
            & _localService

      _remoteRelays = [_remote_TCP_Relay, _remote_UDP_Relay]
      _localServices = _localService_TCP_Forwards
                    <> _localService_UDP_Forwards
                    <> pure _localService_SOCKS5
  in

  map RemoteRelayJob _remoteRelays
  <> map LocalServiceJob _localServices

filterJobs :: Options -> [Job] -> [Job]
filterJobs aOptions =
  case aOptions ^. runningMode of
    DebugMode -> id
    RemoteMode -> filter - is _RemoteRelayJob
    LocalMode -> filter - is _LocalServiceJob
