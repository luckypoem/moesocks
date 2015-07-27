{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.MoeSocks.App where

import "cipher-aes" Crypto.Cipher.AES
import Control.Concurrent
import Control.Exception
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
import System.IO.Streams.ByteString
import System.IO.Streams.Network
import System.Random
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
  let portNumber16 = fromWord8 - toListOf both 
                      (aClientRequest ^. portNumber) :: Word16
  in
  case aClientRequest ^. addressType of
    IPv4_address _address -> SockAddrInet 
                              (fromIntegral portNumber16)
                              (fromWord8 - reverse _address)

    Domain_name x -> SockAddrUnix -  x ^. TS.utf8 . _Text
    IPv6_address xs -> 
                        let rs = reverse - xs
                        in
                        SockAddrInet6 
                          (fromIntegral portNumber16)
                          0
                          ( fromWord8 - P.take 4 - rs
                          , fromWord8 - P.drop 4 - P.take 4 - rs
                          , fromWord8 - P.drop 8 - P.take 4 - rs
                          , fromWord8 - P.drop 12 - rs
                          )
                          0


localRequestHandler:: MoeConfig -> Socket -> IO ()
localRequestHandler config aSocket = do
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

  tryParse - do
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

      let _initSocket = socket AF_INET Stream defaultProtocol
      
      logSocket "Connect remote" _initSocket - \_remoteSocket -> do
        let _c = config
        tryAddr (_c ^. remote) (_c ^. remotePort) - \_remoteAddr -> do
          connect _remoteSocket _remoteAddr

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

                _stdGen <- newStdGen

                let _iv = S.pack - P.take _BlockSize - randoms _stdGen
                
                pushStream remoteOutputStream - B.byteString _iv
                
                (_encrypt, _decrypt) <- getCipher
                                          _DefaultMethod
                                          (config ^. password)


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

  tryParse - do
    _iv <- parseFromStream (take _BlockSize) remoteInputStream

    let 
        _aesKey = aesKey aConfig
        _encrypt = encryptCTR _aesKey _iv
        _decrypt = decryptCTR _aesKey _iv
    
    _headerBlock <- _decrypt <$> readExactly (fromIntegral _PacketSize)
                          remoteInputStream
    
    _clientRequest <- 
      case eitherResult - parse requestParser _headerBlock of
        Left err -> throwIO - ParseException err
        Right r -> pure r
    

    let
        connectTarget :: ClientRequest -> IO (Maybe Socket)
        connectTarget _clientRequest = do
          let _socketAddr = addressType_To_SockAddr _clientRequest
          
              connectionType_To_SocketType :: ConnectionType -> SocketType
              connectionType_To_SocketType TCP_IP_stream_connection = Stream
              connectionType_To_SocketType TCP_IP_port_binding = NoSocketType
              connectionType_To_SocketType UDP_port = Datagram
                 
              _socketType = connectionType_To_SocketType -
                              _clientRequest ^. connectionType


          let hints = defaultHints
                        {
                          addrSocketType = _socketType
                        }
          
              _hostName = sockAddr_To_Host _socketAddr
              _port = sockAddr_To_Port _socketAddr

          _addrInfoList <-  getAddrInfo 
                        (Just hints)
                        (Just - _hostName)
                        (Just - _port)

          let _maybeAddrInfo = preview traverse -
                                _addrInfoList
          
          case _maybeAddrInfo of
            Nothing -> return Nothing
            Just _addrInfo -> do
                _targetSocket <- initSocketForType 
                                    (addrAddress _addrInfo)
                                    (addrSocketType _addrInfo)

                connect _targetSocket - addrAddress _addrInfo
                pure - Just _targetSocket

    _targetSocket <- connectTarget _clientRequest
    
    forM_ _targetSocket - \_targetSocket ->
      logSocket "Connect target" (pure _targetSocket) - \_targetSocket -> do
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

              targetInputBlockStream <- tokenizeStream _PacketSize
                                        _encrypt targetInputStream
              
              remoteInputBlockStream <- detokenizeStream _PacketSize
                                        _decrypt remoteInputStream

              waitBoth
                (Stream.connect remoteInputBlockStream targetOutputStream)
                (Stream.connect targetInputBlockStream remoteOutputStream)
              
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
    let localApp :: SockAddr -> IO ()
        localApp _localAddr = do
          putStrLn "Moe local!"
          
          logSocket "Local loop" (initSocket _localAddr) - \localSocket -> do
            setSocketOption localSocket ReuseAddr 1
            bindSocket localSocket _localAddr

            listen localSocket 1

            let handleLocal _socket = do
                  (_newSocket, _) <- accept _socket
                  forkIO - catchAll - 
                            logSocket "Local handler" (pure _newSocket) -
                              localRequestHandler config

            forever - handleLocal localSocket

    let remoteApp :: SockAddr -> IO ()
        remoteApp _remoteAddr = do
          putStrLn "Moe remote!"

          logSocket "Remote Loop" (initSocket _remoteAddr) - \remoteSocket -> 
            do

            setSocketOption remoteSocket ReuseAddr 1
            bindSocket remoteSocket _remoteAddr
            listen remoteSocket 1

            let handleRemote _socket = do
                  (_newSocket, _) <- accept _socket
                  forkIO - catchAll - 
                              logSocket "Remote handler" (pure _newSocket) -
                                remoteRequestHandler config 

            forever - handleRemote remoteSocket

    let 
        remoteRun :: IO ()
        remoteRun = do
          let _c = config
          tryAddr (_c ^. remote) (_c ^. remotePort) - 
            catchAllLog "remote" . remoteApp 
          
        localRun :: IO ()
        localRun = do
          let _c = config
          tryAddr (_c ^. local) (_c ^. localPort) - 
            catchAllLog "local" . localApp 

        debugRun :: IO ()
        debugRun = do
          catchAllLog "both" - waitBoth localRun remoteRun


    case options ^. runningMode of
      DebugMode -> debugRun
      RemoteMode -> remoteRun
      LocalMode -> localRun

    
