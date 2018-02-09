{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.MoeSocks.Handler where

import Control.Concurrent (forkIO)
import Control.Lens
import Control.Monad.Writer (forever, when)
import Data.Monoid ((<>))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Lens
import Network.Socket (Socket, SockAddr, SocketType(Stream, Datagram), bind)
import Network.Socket (SocketOption(ReuseAddr), setSocketOption, accept, maxListenQueue)
import Network.Socket (listen)
import Network.Socket.ByteString (recvFrom)

import Network.MoeSocks.Common (setSocketConfig)
import Network.MoeSocks.TCP (local_SOCKS5_RequestHandler, local_TCP_ForwardRequestHandler)
import Network.MoeSocks.TCP (remote_TCP_RequestHandler)
import Network.MoeSocks.Type
import Network.MoeSocks.UDP (local_UDP_ForwardRequestHandler, remote_UDP_RequestHandler)

import Network.MoeSocks.Helper
import Prelude hiding ((-), take)

_PacketLength :: Int
_PacketLength = 4096

data ServiceType = TCP_Service | UDP_Service
  deriving (Show, Eq)

localService :: Env
                -> ServiceType
                -> String
                -> (ByteString -> (Socket, SockAddr) -> IO ())
                -> (Socket, SockAddr)
                -> IO ()
localService _env aServiceType aID aHandler s =
  logSA "L loop" (pure s) - \(_localSocket, _localAddr) -> do

    setSocketOption _localSocket ReuseAddr 1
    bind _localSocket _localAddr

    case aServiceType of
      TCP_Service -> do
        info_ - "LT: " <> aID <> " nyaa!"

        when (_env ^. fastOpen) -
          setSocket_TCP_FAST_OPEN _localSocket

        listen _localSocket maxListenQueue

        let handleLocal _socket = do
              _s@(_newSocket, _newSockAddr) <- accept _socket
              setSocketCloseOnExec _newSocket
              setSocketConfig _env _newSocket

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


showWrapped :: (Show a) => a -> String
showWrapped x = "[" <> show x <> "]"

local_SOCKS5 :: Env -> Text -> Int -> (Socket, SockAddr) -> IO ()
local_SOCKS5 _env _remoteHost _remotePort _s =
  localService _env TCP_Service
                ("SOCKS5 proxy " <> showWrapped (_s ^. _2))
                (local_SOCKS5_RequestHandler _env
                                              _remoteHost
                                              _remotePort) - _s

showForwarding :: Forward -> String
showForwarding (Forward _localPort _remoteHost _remotePort) =
                    "["
                <> show _localPort
                <> " -> "
                <> _remoteHost ^. _Text
                <> ":"
                <> show _remotePort
                <> "]"


localForward_TCP :: Env -> Text -> Int -> Forward -> (Socket, SockAddr)
                                                              -> IO ()
localForward_TCP _env _remoteHost _remotePort _f _s = do
  let _m = showForwarding _f
  localService _env TCP_Service ("TCP port forwarding " <> _m)
                          (local_TCP_ForwardRequestHandler
                            _env
                            _remoteHost
                            _remotePort
                            _f)
                          _s

localForward_UDP :: Env -> Text -> Int -> Forward -> (Socket, SockAddr)
                                                              -> IO ()
localForward_UDP _env _remoteHost _remotePort _f _s = do
  let _m = showForwarding _f
  localService _env UDP_Service ("UDP port forwarding " <> _m)
                          (local_UDP_ForwardRequestHandler
                            _env
                            _remoteHost
                            _remotePort
                            _f)
                          _s

remote_TCP_Relay :: Env -> (Socket, SockAddr) -> IO ()
remote_TCP_Relay _env s = logSA "R loop" (pure s) -
  \(_remoteSocket, _remoteAddr) -> do
    info_ - "RT: TCP relay " <> showWrapped _remoteAddr <> " nyaa!"

    setSocketOption _remoteSocket ReuseAddr 1
    bind _remoteSocket _remoteAddr

    when (_env ^. fastOpen) -
      setSocket_TCP_FAST_OPEN _remoteSocket

    listen _remoteSocket maxListenQueue

    let handleRemote _socket = do
          (_newSocket, _) <- accept _socket
          setSocketCloseOnExec _newSocket
          setSocketConfig _env _newSocket

          forkIO - catchExceptAsyncLog "RT" -
                      logSocket "R remote socket" (pure _newSocket) -
                        remote_TCP_RequestHandler _env

    forever - handleRemote _remoteSocket

remote_UDP_Relay :: Env -> (Socket, SockAddr) -> IO ()
remote_UDP_Relay _env s = logSA "R loop" (pure s) -
  \(_remoteSocket, _remoteAddr) -> do
    info_ - "RU: UDP relay " <> showWrapped _remoteAddr <> " nyaa!"

    setSocketOption _remoteSocket ReuseAddr 1
    bind _remoteSocket _remoteAddr

    let handleRemote = do
          (_msg, _sockAddr) <- recvFrom _remoteSocket _PacketLength

          debug_ - "R UDP: " <> show _msg

          let _s = (_remoteSocket, _sockAddr)


          forkIO - catchExceptAsyncLog "RU" -
                      remote_UDP_RequestHandler _env _msg _s

    forever handleRemote


runRemoteRelay :: Env -> RemoteRelay -> IO ()
runRemoteRelay _env _remoteRelay = do
  let _address = _remoteRelay ^. remoteRelayHost
      _port = _remoteRelay ^. remoteRelayPort

  case _remoteRelay ^. remoteRelayType of
    Remote_TCP_Relay -> catchExceptAsyncLog "R TCP Relay" - do
                          getSocket _address _port Stream
                            >>= remote_TCP_Relay _env

    Remote_UDP_Relay  -> catchExceptAsyncLog "R UDP Relay" - do
                          getSocket _address _port Datagram
                            >>= remote_UDP_Relay _env


runLocalService :: Env -> LocalService -> IO ()
runLocalService _env _localService = do
  case _localService ^. localServiceType of
    LocalService_TCP_Forward forwarding ->
      catchExceptAsyncLog "L TCP_Forwarding" - do
        getSocket (_localService ^. localServiceHost)
          (forwarding ^. forwardLocalPort)
          Stream
        >>= localForward_TCP _env
                            (_localService ^. localServiceRemoteHost)
                            (_localService ^. localServiceRemotePort)
                            forwarding

    LocalService_UDP_Forward forwarding ->
      catchExceptAsyncLog "L UDP_Forwarding" - do
        getSocket (_localService ^. localServiceHost)
          (forwarding ^. forwardLocalPort)
          Datagram
        >>= localForward_UDP _env
                            (_localService ^. localServiceRemoteHost)
                            (_localService ^. localServiceRemotePort)
                            forwarding

    LocalService_SOCKS5 _port ->
      catchExceptAsyncLog "L SOCKS5" - do
        getSocket (_localService ^. localServiceHost) _port Stream
          >>= local_SOCKS5 _env
                            (_localService ^. localServiceRemoteHost)
                            (_localService ^. localServiceRemotePort)
