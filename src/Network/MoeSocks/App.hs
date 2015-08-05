{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Network.MoeSocks.App where


import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader hiding (local)
import Control.Monad.Writer hiding (listen)
import Data.Aeson hiding (Result)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
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
import Control.Exception
import Data.Maybe (isNothing)

import OpenSSL (withOpenSSL)
import OpenSSL.EVP.Cipher (getCipherByName)

showAddressType :: AddressType -> Text
showAddressType (IPv4_address xs) = view (from _Text) - 
                                      concat - L.intersperse "." - 
                                      map show - xs ^.. each
showAddressType (Domain_name x)   = x 
showAddressType x                 = error -
                                            "IPv6 target not supported:"
                                            <> show x

showConnectionType :: ConnectionType -> String
showConnectionType TCP_IP_stream_connection = "TCP_Stream"
showConnectionType TCP_IP_port_binding      = "TCP_Bind  "
showConnectionType UDP_port                 = "UDP       "

processLocalSocks5Request :: Socket -> IO (ClientRequest, ByteString)
processLocalSocks5Request aSocket = do
  (_partialBytesAfterGreeting, r) <- 
      parseSocket "clientGreeting" mempty pure greetingParser aSocket

  when (not - _No_authentication `elem` (r ^. authenticationMethods)) - 
    throwIO - ParseException
               "Client does not support no authentication method"

  send_ aSocket - builder_To_ByteString greetingReplyBuilder 

  (_partialBytesAfterClientRequest, _clientRequest) <- parseSocket 
                                "clientRequest" 
                                _partialBytesAfterGreeting
                                pure
                                connectionParser
                                aSocket

  puts - "L : " <> show _clientRequest

  pure - (_clientRequest, _partialBytesAfterClientRequest)

localSocks5RequestHandler :: MoeConfig -> Socket -> IO ()
localSocks5RequestHandler aConfig aSocket = do
  _r <- processLocalSocks5Request aSocket 
  localRequestHandler _r aConfig aSocket

localRequestHandler :: (ClientRequest, ByteString) -> 
                        MoeConfig -> Socket -> IO ()
localRequestHandler (_clientRequest, _partialBytesAfterClientRequest) 
                    aConfig aSocket = do
  let 
      _c = aConfig 
      _initSocket = 
          getSocket (_c ^. remote) (_c ^. remotePort) Stream 

  logSA "L remote socket" _initSocket - 
    \(_remoteSocket, _remoteAddress) -> do
    connect _remoteSocket _remoteAddress

    _localPeerAddr <- getPeerName aSocket
    _remoteSocketName <- getSocketName _remoteSocket

    let _connectionReplyBuilder = connectionReplyBuilder _remoteSocketName

    send_ aSocket - builder_To_ByteString _connectionReplyBuilder
    
    let showRequest :: ClientRequest -> String
        showRequest _r =  
                           view _Text (showAddressType (_r ^. addressType))
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
          
          sendChannel <- newChan
          receiveChannel <- newChan

          let sendThread = do
                sendBuilderEncrypted 
                  sendChannel _encrypt _header

                when (_partialBytesAfterClientRequest & isn't _Empty) -
                  writeChan sendChannel =<< 
                    _encrypt _partialBytesAfterClientRequest

                let _produce = produceLoop 
                                  aSocket 
                                  sendChannel 
                                  _encrypt

                let _consume = consumeLoop 
                                  __remoteSocket 
                                  sendChannel

                runBoth _produce _consume


          let receiveThread = do
                let _produce = produceLoop 
                                  __remoteSocket 
                                  receiveChannel
                                  _decrypt

                let _consume = consumeLoop 
                                  aSocket 
                                  receiveChannel

                runBoth _produce _consume

          runBothDebug
            (Just "L -->", sendThread)
            (Just "L <--", receiveThread)


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
          sendChannel <- newChan
          receiveChannel <- newChan

          let sendThread = do
                when (_leftOverBytes & isn't _Empty) -
                  writeChan sendChannel _leftOverBytes

                let _produce = produceLoop
                                aSocket
                                sendChannel
                                _decrypt

                let _consume = consumeLoop 
                                  __targetSocket
                                  sendChannel

                runBoth _produce _consume


          let receiveThread = do
                let _produce = produceLoop
                                  __targetSocket
                                  receiveChannel
                                  _encrypt

                let _consume = consumeLoop
                                  aSocket
                                  receiveChannel

                runBoth _produce _consume

          runBothDebug
            (Just "R -->", sendThread)
            (Just "R <--", receiveThread)
          
    handleTarget _leftOverBytes _targetSocket

parseConfig :: Text -> MoeMonadT MoeConfig
parseConfig aFilePath = do
  _configFile <- io - TIO.readFile - aFilePath ^. _Text

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
  
      _maybeConfig = _v >>= decode . encode . fixConfig :: Maybe MoeConfig


      formatConfig :: Value -> Value
      formatConfig (Object _obj) =
          Object -
            _obj & H.toList &
                over (mapped . _1) T.tail & H.fromList
      formatConfig _ = Null



  case _maybeConfig of
    Nothing -> do
      let _r = 
            execWriter - do
              tell "\n\n"
              tell "Failed to parse configuration file\n"
              tell "Example: \n"

              let configBS :: ByteString  
                  configBS = toStrict .encode . formatConfig . toJSON - 
                                defaultMoeConfig
              
              tell - configBS ^. utf8 <> "\n"

      throwError - _r ^. _Text 

    Just _config -> do
      pure - _config 


initLogger :: Priority -> IO ()
initLogger aLevel = do
  stdoutHandler <- streamHandler IO.stdout DEBUG
  let formattedHandler = 
          LogHandler.setFormatter stdoutHandler -
            simpleLogFormatter "$time $msg"

  updateGlobalLogger rootLoggerName removeHandler

  updateGlobalLogger "moe" removeHandler
  updateGlobalLogger "moe" - addHandler formattedHandler
  updateGlobalLogger "moe" - setLevel aLevel

moeApp:: MoeMonadT ()
moeApp = do
  _options <- ask 
  io - initLogger - _options ^. verbosity
  
  _config <- parseConfig - _options ^. configFile

  let _method = _config ^. method . _Text

  _cipher <- io - withOpenSSL - getCipherByName _method

  case _cipher of
    Nothing -> throwError - "Invalid method '" 
                            <> _method
                            <> "' in "
                            <> _options ^. configFile . _Text
    Just _ -> pure ()

  let localAppBuilder :: (Socket -> IO ()) -> (Socket, SockAddr) -> IO ()
      localAppBuilder aHandler s = logSA "L loop" (pure s) - 
        \(_localSocket, _localAddr) -> do
          _say "L : nyaa!"
            
          setSocketOption _localSocket ReuseAddr 1
          bindSocket _localSocket _localAddr

          listen _localSocket 1

          let handleLocal _socket = do
                (_newSocket, _) <- accept _socket
                setSocketCloseOnExec _newSocket
                
                forkIO - catchExceptAsyncLog "L thread" - 
                          logSocket "L client socket" (pure _newSocket) -
                            aHandler

          forever - handleLocal _localSocket

  let localSocks5App :: (Socket, SockAddr) -> IO ()
      localSocks5App = localAppBuilder - localSocks5RequestHandler _config

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
                setSocketCloseOnExec _newSocket
                
                forkIO - catchExceptAsyncLog "R thread" - 
                            logSocket "R remote socket" (pure _newSocket) -
                              remoteRequestHandler _config 

          forever - handleRemote _remoteSocket

  let 
      remoteRun :: IO ()
      remoteRun = do
        let _c = _config
        getSocket (_c ^. remote) (_c ^. remotePort) Stream
          >>= catchExceptAsyncLog "R app" . remoteApp 
        
      localRun :: IO ()
      localRun = do
        let _c = _config
        getSocket (_c ^. local) (_c ^. localPort) Stream
          >>= catchExceptAsyncLog "L app" . localSocks5App 

      debugRun :: IO ()
      debugRun = do
        catchExceptAsyncLog "Debug app" - do
          runBoth localRun remoteRun

  io - case _options ^. runningMode of
    DebugMode -> debugRun
    RemoteMode -> remoteRun
    LocalMode -> localRun

