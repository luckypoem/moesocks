{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Network.MoeSocks.App where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Data.Aeson
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
import Network.Socket
import Prelude hiding ((-), take)
import System.IO.Streams.Attoparsec
import System.IO.Streams.Network
import qualified Data.ByteString.Builder as B
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.IO.Streams as Stream

import qualified System.Log.Handler as LogHandler
import System.Log.Handler.Simple
import System.Log.Formatter
import System.Log.Logger
import qualified System.IO as IO

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
  (inputStream, outputStream) <- socketToStreams aSocket

  r <- parseFromStream greetingParser inputStream
  {-puts - "greetings: " <> show r-}

  forM_ (boolToMaybe - 
          _No_authentication `elem` (r ^. authenticationMethods)) - const -
    do
    pushStream outputStream - greetingReplyBuilder 

    _clientRequest <- parseFromStream connectionParser inputStream
    puts - "L : " <> show _clientRequest

    let 
        _c = aConfig 
        _initSocket = 
            getSocket (_c ^. remote . _Text) (_c ^. remotePort) Stream 
    
    logSA "L connect remote" _initSocket - 
      \(_remoteSocket, _remoteAddress) -> do

                          
      connect _remoteSocket _remoteAddress

      _localPeerAddr <- getPeerName aSocket

      _remoteSocketName <- getSocketName _remoteSocket

      {-puts - "remoteSocketName: " <> show _remoteSocketName-}
      {-puts- "socket pair: " <> show (sockAddr_To_Pair _remoteSocketName)-}

      let _connectionReplyBuilder = connectionReplyBuilder _remoteSocketName

      Stream.write (Just - builder_To_ByteString _connectionReplyBuilder)
                    outputStream
      
      let showRequest :: ClientRequest -> String
          showRequest _r =  
                            showAddressType (_r ^. addressType)
                          <> ":"
                          <> show (_r ^. portNumber)
      _log - "L " <> showConnectionType (_clientRequest ^. connectionType)
                  <> ": " <>
              (
                concat - L.intersperse " -> " 
                [ 
                  show _localPeerAddr
                , showRequest _clientRequest
                ]
              )

      let handleLocal __remoteSocket = do

            (remoteInputStream, remoteOutputStream) <- 
              socketToStreams _remoteSocket

            (_encrypt, _decrypt) <- getCipher
                                      (aConfig ^. method)
                                      (aConfig ^. password)


            let 
                _header = shadowsocksRequestBuilder _clientRequest
            
            remoteOutputEncryptedStream <-
              Stream.contramapM _encrypt remoteOutputStream 
            
            pushStream remoteOutputEncryptedStream - 
                B.byteString - builder_To_ByteString _header
            
            remoteInputDecryptedStream <-
              Stream.mapM _decrypt remoteInputStream
            
            let
                sendChannel = 
                    Stream.connect inputStream remoteOutputEncryptedStream
            
            doneFlag <- newEmptyMVar
            
            
            let receiveChannel =  connectFor doneFlag
                                    remoteInputDecryptedStream
                                    outputStream

            waitOneDebug 
              (Just "L -->", sendChannel)
              (Just "L <--", receiveChannel)
              (setDone doneFlag)


      handleLocal _remoteSocket


remoteRequestHandler:: MoeConfig -> Socket -> IO ()
remoteRequestHandler aConfig aSocket = do
  (remoteInputStream, remoteOutputStream) <- socketToStreams aSocket

  (_encrypt, _decrypt) <- getCipher
                            (aConfig ^. method)
                            (aConfig ^. password)
  
  remoteInputDecryptedStream <- Stream.mapM _decrypt remoteInputStream
                                          
  _clientRequest <- parseFromStream 
                      shadowsocksRequestParser remoteInputDecryptedStream

  {-
   -puts - "Remote get: " <> show _clientRequest
   -}
  
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

  logSA "R connect target" (initTarget _clientRequest) - \_r -> do
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
        handleTarget __targetSocket = do
          (targetInputStream, targetOutputStream) <- 
            socketToStreams _targetSocket

          remoteOutputEncryptedStream <- 
            Stream.contramapM _encrypt remoteOutputStream

          let sendChannel = 
                Stream.connect remoteInputDecryptedStream targetOutputStream


          doneFlag <- newEmptyMVar

          let receiveChannel = 
                connectFor doneFlag 
                  targetInputStream remoteOutputEncryptedStream

          waitOneDebug 
            (Just "R -->", sendChannel)
            (Just "R <--", receiveChannel)
            (setDone doneFlag)
          
    handleTarget _targetSocket

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

  {-puts - "stdoutHandler Level" <> -}
          {-show (LogHandler.getLevel stdoutHandler)-}

  let formattedHandler = 
          LogHandler.setFormatter stdoutHandler -
            simpleLogFormatter "[$time $prio] $msg"

  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger "moe" - addHandler formattedHandler

  updateGlobalLogger "moe" - setLevel (options ^. verbosity)
      

  maybeConfig <- parseConfig - options ^. configFile 

  forM_ maybeConfig - \config -> do
    let localApp :: (Socket, SockAddr) -> IO ()
        localApp s = logSA "L loop" (pure s) - 
          \(_localSocket, _localAddr) -> do
            puts "Moe local!"
              
            setSocketOption _localSocket ReuseAddr 1
            bindSocket _localSocket _localAddr

            listen _localSocket 1

            let handleLocal _socket = do
                  (_newSocket, _) <- accept _socket
                  forkIO - catchAllLog "L thread" - 
                            logSocket "L handler" (pure _newSocket) -
                              localRequestHandler config

            forever - handleLocal _localSocket

    let remoteApp :: (Socket, SockAddr) -> IO ()
        remoteApp s = logSA "R loop" (pure s) -
          \(_remoteSocket, _remoteAddr) -> do
          puts "Moe remote!"

          setSocketOption _remoteSocket ReuseAddr 1
          bindSocket _remoteSocket _remoteAddr

          let _maximum_number_of_queued_connection = 1

          listen _remoteSocket _maximum_number_of_queued_connection 

          let handleRemote _socket = do
                (_newSocket, _) <- accept _socket
                forkIO - catchAllLog "R thread" - 
                            logSocket "R handler" (pure _newSocket) -
                              remoteRequestHandler config 

          forever - handleRemote _remoteSocket

    let 
        remoteRun :: IO ()
        remoteRun = do
          let _c = config
          getSocket (_c ^. remote . _Text) (_c ^. remotePort) Stream
            >>= catchAllLog "R app" . remoteApp 
          
        localRun :: IO ()
        localRun = do
          let _c = config
          getSocket (_c ^. local . _Text) (_c ^. localPort) Stream
            >>= catchAllLog "L app" . localApp 

        debugRun :: IO ()
        debugRun = do
          catchAllLog "Debug app" - do
            {-puts "Waiting ..."-}
            {-threadDelay 1000000 -- wait last instance terminate-}
            {-puts "Done"-}

            runBoth localRun remoteRun


    case options ^. runningMode of
      DebugMode -> debugRun
      RemoteMode -> remoteRun
      LocalMode -> localRun

