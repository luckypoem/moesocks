{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Network.MoeSocks.App where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Data.Aeson hiding (Result)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Monoid
import Data.Text (Text)
import Data.Text.Lens
import Data.Text.Strict.Lens (utf8)
import Network.MoeSocks.BuilderAndParser
import Network.MoeSocks.Config
import Network.MoeSocks.Constant
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
import Network.Socket hiding (send, recv)
import Prelude hiding ((-), take)
import System.Log.Formatter
import System.Log.Handler.Simple
import System.Log.Logger
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.IO as IO
import qualified System.Log.Handler as LogHandler

showAddressType :: AddressType -> String
showAddressType (IPv4_address xs) = concat - L.intersperse "." - 
                                      map show - xs ^.. each
showAddressType (Domain_name x)   = x ^. _Text
showAddressType x                 = error -
                                            "IPv6 target not supported:"
                                            <> show x

showConnectionType :: ConnectionType -> String
showConnectionType TCP_IP_stream_connection = "TCP_Stream"
showConnectionType TCP_IP_port_binding      = "TCP_Bind  "
showConnectionType UDP_port                 = "UDP       "

localRequestHandler:: MoeConfig -> Socket -> IO ()
localRequestHandler aConfig aSocket = do
  (_partialBytesAfterGreeting, r) <- 
      parseSocket "clientGreeting" mempty pure greetingParser aSocket

  forM_ (boolToMaybe - 
          _No_authentication `elem` (r ^. authenticationMethods)) - const -
    do
    sendBuilder aSocket greetingReplyBuilder 

    (_partialBytesAfterClientRequest, _clientRequest) <- parseSocket 
                                  "clientRequest" 
                                  _partialBytesAfterGreeting
                                  pure
                                  connectionParser
                                  aSocket

    puts - "L : " <> show _clientRequest
    
    let 
        _c = aConfig 
        _initSocket = 
            getSocket (_c ^. remote . _Text) (_c ^. remotePort) Stream 

    logSA "L remote socket" _initSocket - 
      \(_remoteSocket, _remoteAddress) -> do
      connect _remoteSocket _remoteAddress

      _localPeerAddr <- getPeerName aSocket
      _remoteSocketName <- getSocketName _remoteSocket

      let _connectionReplyBuilder = connectionReplyBuilder _remoteSocketName

      sendBuilder aSocket _connectionReplyBuilder
      
      let showRequest :: ClientRequest -> String
          showRequest _r =  
                            showAddressType (_r ^. addressType)
                          <> ":"
                          <> show (_r ^. portNumber)
      _log - "L " -- <> showConnectionType (_clientRequest ^. connectionType)
                  <> ": " <>
              (
                concat - L.intersperse " -> " 
                [ 
                  show _localPeerAddr
                , showRequest _clientRequest
                ]
              )

      let handleLocal __remoteSocket = do
            (_encrypt, _decrypt) <- getCipher
                                      (aConfig ^. method)
                                      (aConfig ^. password)


            let 
                _header = shadowSocksRequestBuilder _clientRequest
            
            let sendChannel = do
                  sendBuilderEncrypted _encrypt __remoteSocket _header

                  when (_partialBytesAfterClientRequest & isn't _Empty) -
                    send_ __remoteSocket =<< 
                      _encrypt _partialBytesAfterClientRequest

                  let sendChannelLoop = do 
                        _r <- recv_ aSocket
                        if (_r & isn't _Empty) 
                          then do
                            send_ __remoteSocket =<< _encrypt _r
                            sendChannelLoop
                          else do
                            puts - "0 bytes from remote!"
                            close aSocket

                  sendChannelLoop

            let receiveChannel = do
                  _r <- recv_ __remoteSocket
                  if (_r & isn't _Empty) 
                    then do
                      send_ aSocket =<< _decrypt _r
                      receiveChannel
                    else do
                      puts - "0 bytes from target!"
                      close __remoteSocket

            runBothDebug
              (Just "L -->", sendChannel)
              (Just "L <--", receiveChannel)


      handleLocal _remoteSocket


remoteRequestHandler:: MoeConfig -> Socket -> IO ()
remoteRequestHandler aConfig aSocket = do
  (_encrypt, _decrypt) <- getCipher
                            (aConfig ^. method)
                            (aConfig ^. password)
  
  (_leftOverBytes, _clientRequest) <- parseSocket 
                                          "clientRequest"
                                          mempty
                                          _decrypt
                                          shadowSocksRequestParser 
                                          aSocket
                                          

  puts - "Remote get: " <> show _clientRequest
  
  let
      initTarget :: ClientRequest -> IO (Socket, SockAddr)
      initTarget _clientRequest = do
        let 
            connectionType_To_SocketType :: ConnectionType -> SocketType
            connectionType_To_SocketType TCP_IP_stream_connection = Stream
            connectionType_To_SocketType TCP_IP_port_binding = NoSocketType
            connectionType_To_SocketType UDP_port = Datagram
               
            _socketType = connectionType_To_SocketType -
                            _clientRequest ^. connectionType


            _hostName = _clientRequest ^. addressType . to showAddressType
            _port = _clientRequest ^. portNumber

        
        getSocket _hostName _port _socketType

  logSA "R target socket" (initTarget _clientRequest) - \_r -> do
    let (_targetSocket, _targetSocketAddress) = _r 

    connect _targetSocket _targetSocketAddress

    _remotePeerAddr <- getPeerName aSocket
    _targetPeerAddr <- getPeerName _targetSocket

    _log - "R " -- <> showConnectionType (_clientRequest ^. connectionType)
                  <> ": " <>
            (
              concat - L.intersperse " -> " - map show
              [ 
                _remotePeerAddr
              , _targetPeerAddr
              ]
            )
    let 
        handleTarget __leftOverBytes __targetSocket = do
          let sendChannel = do
                when (__leftOverBytes & isn't _Empty) -
                  send_ __targetSocket _leftOverBytes

                let sendChannelLoop = do 
                      r <- recv_ aSocket
                      if (r & isn't _Empty) 
                        then do
                          send_ __targetSocket =<< _decrypt r
                          sendChannelLoop
                        else do
                          puts - "0 bytes from remote!"
                          close aSocket

                sendChannelLoop

          let receiveChannel = do
                r <- recv_ __targetSocket
                if (r & isn't _Empty) 
                  then do
                    send_ aSocket =<< _encrypt r
                    receiveChannel
                  else do
                    puts - "0 bytes from target!"
                    close __targetSocket

          runBothDebug
            (Just "R -->", sendChannel)
            (Just "R <--", receiveChannel)
          
    handleTarget _leftOverBytes _targetSocket

parseConfig :: Text -> IO (Maybe MoeConfig)
parseConfig aConfigFile = do
  _configFile <- TIO.readFile - aConfigFile ^. _Text

  let 
      fromShadowSocksConfig :: [(Text, Value)] -> [(Text, Value)]
      fromShadowSocksConfig _configList = 
        let fixes =
              [
                ("server", "remote")
              , ("server_port", "remotePort")
              , ("local_address", "local")
              , ("local_port", "localPort")
              ]

        in
        foldl (flip duplicateKey) _configList fixes

      fromSS :: [(Text, Value)] -> [(Text, Value)]
      fromSS = fromShadowSocksConfig


  let _v = decodeStrict - review utf8 _configFile :: Maybe Value

      fixConfig :: Value -> Value
      fixConfig (Object _obj) =
          Object - 
            _obj & H.toList & fromSS & 
                over (mapped . _1) (T.cons '_')  & H.fromList
      fixConfig _ = Null
  
      _maybeConfig = _v >>= decode . encode . fixConfig


      formatConfig :: Value -> Value
      formatConfig (Object _obj) =
          Object -
            _obj & H.toList &
                over (mapped . _1) T.tail & H.fromList
      formatConfig _ = Null



  case _maybeConfig of
    Nothing -> do
      pute "Failed to parse configuration file"
      pute "Example: "

      let configBS :: ByteString  
          configBS = toStrict .encode . formatConfig . toJSON - 
                        defaultMoeConfig
      
      puteT - configBS ^. utf8

      pure Nothing
    _config -> do
      pure - _config 

moeApp:: MoeOptions -> IO ()
moeApp options = do
  stdoutHandler <- streamHandler IO.stdout DEBUG
  let formattedHandler = 
          LogHandler.setFormatter stdoutHandler -
            simpleLogFormatter "$time $msg"

  updateGlobalLogger rootLoggerName removeHandler

  updateGlobalLogger "moe" removeHandler
  updateGlobalLogger "moe" - addHandler formattedHandler
  updateGlobalLogger "moe" - setLevel - options ^. verbosity
      

  maybeConfig <- parseConfig - options ^. configFile 

  forM_ maybeConfig - \config -> do
    let localApp :: (Socket, SockAddr) -> IO ()
        localApp s = logSA "L loop" (pure s) - 
          \(_localSocket, _localAddr) -> do
            _say "L : nyaa!"
              
            setSocketOption _localSocket ReuseAddr 1
            bindSocket _localSocket _localAddr

            listen _localSocket 1

            let handleLocal _socket = do
                  (_newSocket, _) <- accept _socket
                  forkIO - catchExceptAsyncLog "L thread" - 
                            logSocket "L client socket" (pure _newSocket) -
                              localRequestHandler config

            forever - handleLocal _localSocket

    let remoteApp :: (Socket, SockAddr) -> IO ()
        remoteApp s = logSA "R loop" (pure s) -
          \(_remoteSocket, _remoteAddr) -> do
          _say "R : nyaa!"

          setSocketOption _remoteSocket ReuseAddr 1
          bindSocket _remoteSocket _remoteAddr

          let _maximum_number_of_queued_connection = 1

          listen _remoteSocket _maximum_number_of_queued_connection 

          let handleRemote _socket = do
                (_newSocket, _) <- accept _socket
                forkIO - catchExceptAsyncLog "R thread" - 
                            logSocket "R remote socket" (pure _newSocket) -
                              remoteRequestHandler config 

          forever - handleRemote _remoteSocket

    let 
        remoteRun :: IO ()
        remoteRun = do
          let _c = config
          getSocket (_c ^. remote . _Text) (_c ^. remotePort) Stream
            >>= catchExceptAsyncLog "R app" . remoteApp 
          
        localRun :: IO ()
        localRun = do
          let _c = config
          getSocket (_c ^. local . _Text) (_c ^. localPort) Stream
            >>= catchExceptAsyncLog "L app" . localApp 

        debugRun :: IO ()
        debugRun = do
          catchExceptAsyncLog "Debug app" - do
            runBoth localRun remoteRun


    case options ^. runningMode of
      DebugMode -> debugRun
      RemoteMode -> remoteRun
      LocalMode -> localRun

