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


localRequestHandler:: MoeConfig -> (Socket, SockAddr) -> IO ()
localRequestHandler config (_s, _) = withSocket _s - \aSocket -> do
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
    
    if not - _No_authentication `elem` (r ^. authenticationMethods)
      then do
        close aSocket

      else do
        pushStream outputStream - B.word8 socksVersion
                                <> B.word8 _No_authentication


        _clientRequest <- parseFromStream connectionParser inputStream
        {-puts - "request: " <> show _clientRequest-}

        let conn = _clientRequest

        _remoteSocket <- socket AF_INET Stream defaultProtocol
        
        withSocket _remoteSocket - \_remoteSocket -> do
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

            let handleLocal _remoteSocket = do
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
                  
                  let
                      _aesKey = aesKey config
                      _encrypt = encryptCTR _aesKey _iv
                      _decrypt = decryptCTR _aesKey _iv
                  

                  let 
                      _header = requestBuilder conn
                      _headerBlock = clamp _PacketSize -
                                      builder_To_ByteString _header

                  pushStream remoteOutputStream - B.byteString - 
                                                      _encrypt _headerBlock

                  inputBlockStream <- tokenizeStream _PacketSize
                                      _encrypt inputStream
                  
                  remoteInputBlockStream <- detokenizeStream _PacketSize
                                            _decrypt remoteInputStream

                  waitBoth
                    (Stream.connect inputBlockStream remoteOutputStream)
                    (Stream.connect remoteInputBlockStream outputStream)
                  

            safeSocketHandler "Local Request Handler" 
              handleLocal _remoteSocket


remoteRequestHandler:: MoeConfig -> (Socket, SockAddr) -> IO ()
remoteRequestHandler aConfig (_s, _) = withSocket _s - \aSocket -> do
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
    
    forM_ _targetSocket - flip withSocket - \_targetSocket -> do
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
          handleTarget _targetSocket = do
            (targetInputStream, targetOutputStream) <- 
              socketToStreams _targetSocket

            targetInputBlockStream <- tokenizeStream _PacketSize
                                      _encrypt targetInputStream
            
            remoteInputBlockStream <- detokenizeStream _PacketSize
                                      _decrypt remoteInputStream

            waitBoth
              (Stream.connect remoteInputBlockStream targetOutputStream)
              (Stream.connect targetInputBlockStream remoteOutputStream)
            
      safeSocketHandler "Target Connection Handler" 
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
    let localApp :: SockAddr -> IO (Socket, Socket -> IO ())
        localApp _localAddr = do
          putStrLn "Moe local!"
          
          localSocket <- initSocket _localAddr
          setSocketOption localSocket ReuseAddr 1
          bindSocket localSocket _localAddr

          listen localSocket 1

          let handleLocal _socket = do
                r@(_newSocket, _newSocketAddr) <- accept _socket
                forkIO - catchAll - onException 
                          (localRequestHandler config r) - do
                              pute "local onException" 
                              close _newSocket
              localLoop = 
                forever . 
                safeSocketHandler "Local Connection" handleLocal

          pure (localSocket, localLoop)

    let remoteApp :: SockAddr -> IO (Socket, Socket -> IO ())
        remoteApp _remoteAddr = do
          putStrLn "Moe remote!"

          remoteSocket <- initSocket _remoteAddr
          setSocketOption remoteSocket ReuseAddr 1
          bindSocket remoteSocket _remoteAddr
          listen remoteSocket 1

          let handleRemote _socket = do
                r@(_newSocket, _newSocketAddr) <- accept _socket
                forkIO - catchAll - onException 
                          (remoteRequestHandler config r) - do
                              pute "remote onException" 
                              close _newSocket

              remoteLoop = 
                forever . 
                safeSocketHandler "Remote Connection" handleRemote

          pure (remoteSocket, remoteLoop)

    let debugRun :: IO ()
        debugRun = do
          let _c = config
          tryAddr (_c ^. local) (_c ^. localPort) - \_localAddr -> do
            (localSocket, localLoop) <- localApp _localAddr

            tryAddr (_c ^. remote) (_c ^. remotePort) - \_remoteAddr -> do
              (remoteSocket, remoteLoop) <- remoteApp _remoteAddr
              catchAll - do
                safeSocketHandler "Local Socket" (\_localSocket ->
                  safeSocketHandler "Remote Socket" (\_remoteSocket ->
                    waitBoth 
                      (localLoop _localSocket) 
                      (remoteLoop _remoteSocket)
                      ) remoteSocket) localSocket

        remoteRun :: IO ()
        remoteRun = do
          let _c = config
          tryAddr (_c ^. remote) (_c ^. remotePort) - \_remoteAddr -> do
            (remoteSocket, remoteLoop) <- remoteApp _remoteAddr
            catchAll - 
              safeSocketHandler "Remote Socket" remoteLoop remoteSocket
          
        localRun :: IO ()
        localRun = do
          let _c = config
          tryAddr (_c ^. local) (_c ^. localPort) - \_localAddr -> do
            (localSocket, localLoop) <- localApp _localAddr
            catchAll - 
              safeSocketHandler "Local Socket" localLoop localSocket

    case options ^. runningMode of
      DebugMode -> debugRun
      RemoteMode -> remoteRun
      LocalMode -> localRun

    
