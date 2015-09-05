{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.MoeSocks.App where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async hiding (waitBoth)
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader hiding (local)
import Control.Monad.Writer hiding (listen)
import Data.ByteString (ByteString)
import Data.Text.Lens
import Network.MoeSocks.Common
import Network.MoeSocks.Encrypt (initCipherBox, safeMethods, unsafeMethods)
import Network.MoeSocks.Helper
import Network.MoeSocks.Modules.Resource (loadConfig)
import Network.MoeSocks.Runtime
import Network.MoeSocks.TCP
import Network.MoeSocks.Type
import Network.MoeSocks.UDP
import Network.Socket hiding (send, recv, recvFrom, sendTo)
import Network.Socket.ByteString
import Prelude hiding ((-), take)

_PacketLength :: Int
_PacketLength = 4096

withGateOptions :: Options -> IO a -> IO ()
withGateOptions aOption aIO = do
  if aOption ^. listMethods
    then do
      let _br = putStrLn ""

      _br
      putStrLn "Recommended:"
      itraverse_ (\k _ -> putStrLn - "\t\t" <> k ^. _Text) - safeMethods

      _br
      putStrLn "Supported:"
      itraverse_ (\k _ -> putStrLn - "\t\t" <> k ^. _Text) - unsafeMethods

    else
      () <$ aIO

data ServiceType = TCP_Service | UDP_Service
  deriving (Show, Eq)

moeApp:: MoeMonadT ()
moeApp = do
  _options <- ask
  io - initLogger - _options ^. verbosity

  io - debug_ - show _options

  _config <- loadConfig - _options
  let _c = _config

  let _method = _config ^. method

  _cipherBox <- (io - initCipherBox _method (_config ^. password)) >>= \case
    Nothing -> throwError - "Invalid method '"
                            <> _method ^. _Text
    Just (a, b, c, d) -> pure - CipherBox a b c d

  let _env = Env _options _config _cipherBox


  let localService :: ServiceType
                      -> String
                      -> (ByteString -> (Socket, SockAddr) -> IO ())
                      -> (Socket, SockAddr)
                      -> IO ()
      localService aServiceType aID aHandler s =
        logSA "L loop" (pure s) - \(_localSocket, _localAddr) -> do

          setSocketOption _localSocket ReuseAddr 1
          bindSocket _localSocket _localAddr

          case aServiceType of
            TCP_Service -> do
              info_ - "LT: " <> aID <> " nyaa!"

              when (_c ^. fastOpen) -
                setSocket_TCP_FAST_OPEN _localSocket

              listen _localSocket maxListenQueue

              let handleLocal _socket = do
                    _s@(_newSocket, _newSockAddr) <- accept _socket
                    setSocketCloseOnExec _newSocket
                    setSocketConfig _c _socket

                    forkIO - catchExceptAsyncLog "LT" -
                              logSA "L TCP client socket" (pure _s) -
                                aHandler ""

              forever - handleLocal _localSocket

            UDP_Service -> do
              info_ - "LU: " <> aID <> " nyaa!"
              let handleLocal = do
                    (_msg, _sockAddr) <-
                        recvFrom _localSocket _PacketLength

                    debug_ - "L UDP: " <> show _msg

                    let _s = (_localSocket, _sockAddr)

                    forkIO - catchExceptAsyncLog "LU" -
                                aHandler _msg _s

              forever handleLocal

  let
      showWrapped :: (Show a) => a -> String
      showWrapped x = "[" <> show x <> "]"

  let local_SOCKS5 :: (Socket, SockAddr) -> IO ()
      local_SOCKS5 _s = localService TCP_Service
                            ("SOCKS5 proxy " <> showWrapped (_s ^. _2))
                            (local_SOCKS5_RequestHandler _env) - _s

      showForwarding :: Forward -> String
      showForwarding (Forward _localPort _remoteHost _remotePort) =
                          "["
                      <> show _localPort
                      <> " -> "
                      <> _remoteHost ^. _Text
                      <> ":"
                      <> show _remotePort
                      <> "]"


      localForward_TCP :: Forward -> (Socket, SockAddr) -> IO ()
      localForward_TCP _f _s = do
        let _m = showForwarding _f
        localService TCP_Service ("TCP port forwarding " <> _m)
                                (local_TCP_ForwardRequestHandler _env _f)
                                _s

      localForward_UDP :: Forward -> (Socket, SockAddr) -> IO ()
      localForward_UDP _f _s = do
        let _m = showForwarding _f
        localService UDP_Service ("UDP port forwarding " <> _m)
                                (local_UDP_ForwardRequestHandler _env _f)
                                _s

  let remote_TCP_Relay :: (Socket, SockAddr) -> IO ()
      remote_TCP_Relay s = logSA "R loop" (pure s) -
        \(_remoteSocket, _remoteAddr) -> do
          info_ - "RT: TCP relay " <> showWrapped _remoteAddr <> " nyaa!"

          setSocketOption _remoteSocket ReuseAddr 1
          bindSocket _remoteSocket _remoteAddr

          when (_c ^. fastOpen) -
            setSocket_TCP_FAST_OPEN _remoteSocket

          listen _remoteSocket maxListenQueue

          let handleRemote _socket = do
                (_newSocket, _) <- accept _socket
                setSocketCloseOnExec _newSocket
                setSocketConfig _c _socket

                forkIO - catchExceptAsyncLog "RT" -
                            logSocket "R remote socket" (pure _newSocket) -
                              remote_TCP_RequestHandler _env

          forever - handleRemote _remoteSocket

  let remote_UDP_Relay :: (Socket, SockAddr) -> IO ()
      remote_UDP_Relay s = logSA "R loop" (pure s) -
        \(_remoteSocket, _remoteAddr) -> do
          info_ - "RU: UDP relay " <> showWrapped _remoteAddr <> " nyaa!"

          setSocketOption _remoteSocket ReuseAddr 1
          bindSocket _remoteSocket _remoteAddr

          let handleRemote = do
                (_msg, _sockAddr) <- recvFrom _remoteSocket _PacketLength

                debug_ - "R UDP: " <> show _msg

                let _s = (_remoteSocket, _sockAddr)


                forkIO - catchExceptAsyncLog "RU" -
                            remote_UDP_RequestHandler _env _msg _s

          forever handleRemote

  -- Run

  let runRemoteRelay :: RemoteRelay -> IO ()
      runRemoteRelay _remoteRelay = do
        let _address = _remoteRelay ^. remoteRelayHost
            _port = _remoteRelay ^. remoteRelayPort

        case _remoteRelay ^. remoteRelayType of
          Remote_TCP_Relay -> catchExceptAsyncLog "R TCP Relay" - do
                                getSocket _address _port Stream
                                  >>= remote_TCP_Relay

          Remote_UDP_Relay  -> catchExceptAsyncLog "R UDP Relay" - do
                                getSocket _address _port Datagram
                                  >>= remote_UDP_Relay


  let runLocalService :: LocalService -> IO ()
      runLocalService _localService = do
        case _localService ^. localServiceType of
          LocalService_TCP_Forward forwarding ->
            catchExceptAsyncLog "L TCP_Forwarding" - do
              getSocket (_localService ^. localServiceHost)
                (forwarding ^. forwardLocalPort)
                Stream
              >>= localForward_TCP forwarding

          LocalService_UDP_Forward forwarding ->
            catchExceptAsyncLog "L UDP_Forwarding" - do
              getSocket (_localService ^. localServiceHost)
                (forwarding ^. forwardLocalPort)
                Datagram
              >>= localForward_UDP forwarding

          LocalService_SOCKS5 _port ->
            catchExceptAsyncLog "L SOCKS5" - do
              getSocket (_localService ^. localServiceHost) _port Stream
                >>= local_SOCKS5

      runJob :: Job -> TVar JobStatus -> IO ()
      runJob (RemoteRelayJob x) _ = runRemoteRelay x
      runJob (LocalServiceJob x) _ = runLocalService x

      runApp :: [Job] -> IO ()
      runApp someJobs = do
        _jobs <- (forM someJobs - \_job -> (,) _job
                                            <$> newTVarIO initialJobStatus)
        _asyncs <- mapM (async . foreverRun . (uncurry runJob)) _jobs

        _mainThread <- async - do
          waitAnyCancel - _asyncs

        _uiThread <- async - forever - do
          let _statuses = _jobs ^.. each . _2 :: [TVar JobStatus]
          {-forM_ _statuses - readTVarIO >=> print-}
          sleep 5

        waitAnyCancel [_mainThread, _uiThread]

        pure ()


  io - runApp - filterJobs (_options ^. runningMode) - loadJobs _c _options
