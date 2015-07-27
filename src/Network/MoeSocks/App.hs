{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Network.MoeSocks.App where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Attoparsec.ByteString
import Data.Monoid
import Data.Text (Text)
import Data.Text.Lens
import Data.Word
import Network.MoeSocks.BuilderAndParser
import Network.MoeSocks.Config
import Network.MoeSocks.Constant
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
import Network.Socket
import Prelude hiding ((-), take)
import System.IO.Streams.Attoparsec
import System.IO.Streams.Network
{-import System.IO.Streams.Debug-}
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as B
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Strict.Lens as TS
import qualified Prelude as P
import qualified System.IO.Streams as Stream


addressType_To_SockAddr :: ClientRequest -> SockAddr
addressType_To_SockAddr aClientRequest =
  let port16 = portNumber16 - aClientRequest ^. portNumber
  in
  case aClientRequest ^. addressType of
    IPv4_address _address -> SockAddrInet 
                              (fromIntegral port16)
                              (fromWord8 - reverse _address)

    Domain_name x -> SockAddrUnix -  x ^. TS.utf8 . _Text
    IPv6_address xs -> 
                        let rs = reverse - xs
                        in
                        SockAddrInet6 
                          (fromIntegral port16)
                          0
                          ( fromWord8 - P.take 4 - rs
                          , fromWord8 - P.drop 4 - P.take 4 - rs
                          , fromWord8 - P.drop 8 - P.take 4 - rs
                          , fromWord8 - P.drop 12 - rs
                          )
                          0


localRequestHandler:: MoeConfig -> Socket -> IO ()
localRequestHandler aConfig aSocket = do
  (inputStream, outputStream) <- socketToStreams aSocket

  let socksVersion = 5
      socksHeader = word8 socksVersion
  
  let greetingParser = do
        socksHeader
        let maxNoOfMethods = 5
        _numberOfAuthenticationMethods <- satisfy (<= maxNoOfMethods)

        ClientGreeting <$>
          count (fromIntegral _numberOfAuthenticationMethods) anyWord8

  let connectionParser = do
        socksHeader
        requestParser

  r <- parseFromStream greetingParser inputStream
  {-puts - "greetings: " <> show r-}

  forM_ (boolToMaybe - 
          _No_authentication `elem` (r ^. authenticationMethods)) - const -
    do
    pushStream outputStream - B.word8 socksVersion
                            <> B.word8 _No_authentication


    _clientRequest <- parseFromStream connectionParser inputStream
    {-puts - "request: " <> show _clientRequest-}

    let conn = _clientRequest

    let 
        _c = aConfig 
        _initSocket = 
            getSocket (_c ^. remote . _Text) (_c ^. remotePort) Stream 
    
    logSA "Connect remote" _initSocket - \(_remoteSocket, _remoteAddress) -> do
      connect _remoteSocket _remoteAddress

      _localPeerAddr <- getPeerName aSocket
      {-_localSocketAddr <- getSocketName aSocket-}
      {-_remotePeerAddr <- getPeerName _remoteSocket-}
      {-_remoteSocketAddr <- getSocketName _remoteSocket-}
      let _clientAddr = addressType_To_SockAddr _clientRequest

      puts - "L: " <> 
              (
                concat - L.intersperse " -> " - map show
                [ 
                  _localPeerAddr
                {-, _localSocketAddr-}
                {-, _remotePeerAddr-}
                {-, _remoteSocketAddr-}
                , _clientAddr
                ]
              )

      let handleLocal __remoteSocket = do
            let
              write x = Stream.write (Just - x) outputStream
              push = write . S.singleton

            push socksVersion
            push _Request_Granted 
            push _ReservedByte

            write - builder_To_ByteString -
                addressTypeBuilder (conn ^. addressType)

            traverseOf both push - conn ^. portNumber

            (remoteInputStream, remoteOutputStream) <- 
              socketToStreams _remoteSocket

            (_encrypt, _decrypt) <- getCipher
                                      _DefaultMethod
                                      (aConfig ^. password)


            let 
                _header = requestBuilder conn
            
            remoteOutputEncryptedStream <-
              Stream.contramapM _encrypt remoteOutputStream 
            
            pushStream remoteOutputEncryptedStream - 
                B.byteString - builder_To_ByteString _header
            
            remoteInputDecryptedStream <-
              Stream.mapM _decrypt remoteInputStream

            waitBoth
              (Stream.connect inputStream remoteOutputEncryptedStream)
              (Stream.connect remoteInputDecryptedStream outputStream)
            

      handleLocal _remoteSocket


remoteRequestHandler:: MoeConfig -> Socket -> IO ()
remoteRequestHandler aConfig aSocket = do
  (remoteInputStream, remoteOutputStream) <- socketToStreams aSocket

  (_encrypt, _decrypt) <- getCipher
                            _DefaultMethod
                            (aConfig ^. password)
  
  remoteInputDecryptedStream <- Stream.mapM _decrypt remoteInputStream
  {-debugRemoteInputDecryptedStream <- debugInputBS-}
                              {-"R: remoteInput"-}
                              {-Stream.stderr-}
                              {-remoteInputDecryptedStream-}
                                          
  _clientRequest <- parseFromStream requestParser remoteInputDecryptedStream

  {-
   -puts - "Remote get: " <> show _clientRequest
   -}
  
  let
      initTarget :: ClientRequest -> IO (Socket, SockAddr)
      initTarget _clientRequest = do
        let _socketAddr = addressType_To_SockAddr _clientRequest
        
            connectionType_To_SocketType :: ConnectionType -> SocketType
            connectionType_To_SocketType TCP_IP_stream_connection = Stream
            connectionType_To_SocketType TCP_IP_port_binding = NoSocketType
            connectionType_To_SocketType UDP_port = Datagram
               
            _socketType = connectionType_To_SocketType -
                            _clientRequest ^. connectionType

            showAddressType :: AddressType -> String
            showAddressType (IPv4_address xs) = concat - L.intersperse "." -
                                                  map show xs
            showAddressType (Domain_name x) = view _Text - x ^. TS.utf8
            showAddressType x = error -
                                        "IPv6 target not supported:"
                                        <> show x

            _hostName = _clientRequest ^. addressType . to showAddressType
            _port = _clientRequest ^. portNumber

        
        getSocket _hostName (portNumber16 _port) _socketType

  logSocketWithAddress "Connect target" (initTarget _clientRequest) - \_r -> do
    let (_targetSocket, _targetSocketAddress) = _r 

    connect _targetSocket _targetSocketAddress

    _remotePeerAddr <- getPeerName aSocket
    {-_remoteSocketAddr <- getSocketName aSocket-}
    _targetPeerAddr <- getPeerName _targetSocket
    {-_targetSocketAddr <- getSocketName _targetSocket-}

    puts - "R: " <> 
            (
              concat - L.intersperse " -> " - map show
              [ 
                _remotePeerAddr
              {-, _remoteSocketAddr-}
              , _targetPeerAddr
              {-, _targetSocketAddr-}
              ]
            )
    let 
        handleTarget __targetSocket = do
          (targetInputStream, targetOutputStream) <- 
            socketToStreams _targetSocket

          {-debugTargetInputStream <- debugInputBS-}
                                      {-"R: targetInput"-}
                                      {-Stream.stderr-}
                                      {-targetInputStream-}
                                        
          {-debugTargetOutputStream <- debugOutputBS-}
                                      {-"R: targetOutput"-}
                                      {-Stream.stderr-}
                                      {-targetOutputStream-}

          remoteOutputEncryptedStream <- 
            Stream.contramapM _encrypt remoteOutputStream

          waitBoth
            (Stream.connect remoteInputDecryptedStream targetOutputStream)
            (Stream.connect targetInputStream remoteOutputEncryptedStream)
          
    handleTarget _targetSocket

parseConfig :: Text -> IO (Maybe MoeConfig)
parseConfig aConfigFile = do
  _configFile <- TIO.readFile - aConfigFile ^. _Text

  let _v = decodeStrict - review TS.utf8 _configFile :: Maybe Value
  let fixConfig :: Value -> Value
      fixConfig (Object _obj) =
          Object - 
            _obj & H.toList & over (mapped . _1) (T.cons '_')  & H.fromList
      fixConfig _ = Null
  let 
      _maybeConfig = (_v >>= decode . encode . fixConfig)

  case _maybeConfig of
    Nothing -> do
      pute "Failed to parse configuration file"
      pute "Example: "
      pute - show - encode defaultMoeConfig
      
      pure Nothing
    _config -> do
      pure - _config 

moeApp:: MoeOptions -> IO ()
moeApp options = do
  maybeConfig <- parseConfig - options ^. configFile 
  
  forM_ maybeConfig - \config -> do
    let localApp :: (Socket, SockAddr) -> IO ()
        localApp s = logSA "Local loop" (pure s) - 
          \(_localSocket, _localAddr) -> do
            putStrLn "Moe local!"
              
            setSocketOption _localSocket ReuseAddr 1
            bindSocket _localSocket _localAddr

            listen _localSocket 1

            let handleLocal _socket = do
                  (_newSocket, _) <- accept _socket
                  forkIO - catchAll - 
                            logSocket "Local handler" (pure _newSocket) -
                              localRequestHandler config

            forever - handleLocal _localSocket

    let remoteApp :: (Socket, SockAddr) -> IO ()
        remoteApp s = logSA "Remote loop" (pure s) -
          \(_remoteSocket, _remoteAddr) -> do
          putStrLn "Moe remote!"


          setSocketOption _remoteSocket ReuseAddr 1
          bindSocket _remoteSocket _remoteAddr
          listen _remoteSocket 1

          let handleRemote _socket = do
                (_newSocket, _) <- accept _socket
                forkIO - catchAll - 
                            logSocket "Remote handler" (pure _newSocket) -
                              remoteRequestHandler config 

          forever - handleRemote _remoteSocket

    let 
        remoteRun :: IO ()
        remoteRun = do
          let _c = config
          getSocket (_c ^. remote . _Text) (_c ^. remotePort) Stream
            >>= catchAllLog "remote" . remoteApp 
          
        localRun :: IO ()
        localRun = do
          let _c = config
          getSocket (_c ^. local . _Text) (_c ^. localPort) Stream
            >>= catchAllLog "local" . localApp 

        debugRun :: IO ()
        debugRun = do
          catchAllLog "both" - waitBoth localRun remoteRun


    case options ^. runningMode of
      DebugMode -> debugRun
      RemoteMode -> remoteRun
      LocalMode -> localRun

