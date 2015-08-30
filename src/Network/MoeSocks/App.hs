{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.MoeSocks.App where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader hiding (local)
import Control.Monad.Writer hiding (listen)
import Data.Aeson hiding (Result)
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lens
import Network.MoeSocks.Config
import Network.MoeSocks.Constant
import Network.MoeSocks.Encrypt (initCipherBox)
import Network.MoeSocks.Helper
import Network.MoeSocks.TCP
import Network.MoeSocks.Type
import Network.MoeSocks.UDP
import Network.Socket hiding (send, recv, recvFrom, sendTo)
import Network.Socket.ByteString
import Prelude hiding ((-), take)
import System.Log.Formatter
import System.Log.Handler.Simple
import System.Log.Logger
import qualified Data.HashMap.Strict as H
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.IO as IO
import qualified System.Log.Handler as LogHandler


parseConfig :: MoeOptions -> MoeMonadT MoeConfig
parseConfig aOption = do
  let _maybeFilePath = aOption ^. configFile 

  _v <- case _maybeFilePath of
          Nothing -> pure - Just - Object mempty
          Just _filePath -> fmap (preview _JSON) - 
                            io - TIO.readFile - _filePath ^. _Text

  let 

      asList :: ([(Text, Value)] -> [(Text, Value)]) -> Value -> Value
      asList f = over _Object - H.fromList . f . H.toList 
      
      fromShadowSocksConfig :: Text -> Text
      fromShadowSocksConfig x = 
        let fixes = Map.fromList
              [
                ("server", "remote")
              , ("server_port", "remotePort")
              , ("local_address", "local")
              ]

        in
        fixes ^? ix x & fromMaybe x

      toParsableConfig :: Value -> Value
      toParsableConfig = asList - each . _1 %~  (
                                                  T.cons '_' 
                                                . toCamelCase
                                                . fromShadowSocksConfig
                                                )  

      toReadableConfig :: Value -> Value
      toReadableConfig = asList - each . _1 %~ T.tail 

      showConfig :: MoeConfig -> Text
      showConfig =  review _JSON
                    . toReadableConfig 
                    . review _JSON 

      filterEssentialConfig :: Value -> Value
      filterEssentialConfig = over _Object - \_obj ->
                                foldl (flip H.delete) _obj - 
                                  [
                                    "_password"
                                  ]
          
      insertConfig :: Value -> Value -> Value
      insertConfig (Object _from) = over _Object (_from `H.union`)
      insertConfig _ = const Null

      insertParams :: [(Text, Value)] -> Value -> Value
      insertParams _from = over _Object (H.fromList _from `H.union`)

      fallbackConfig :: Value -> Value -> Value
      fallbackConfig = flip insertConfig

      optionalConfig = filterEssentialConfig - toJSON defaultMoeConfig
      
      _maybeConfig = _v
                      >>= decode 
                          . encode 
                          . fallbackConfig optionalConfig
                          . insertParams (aOption ^. params)
                          . toParsableConfig 

  case _maybeConfig of
    Nothing -> do
      let _r = 
            execWriter - do
              tell "\n\n"
              case _maybeFilePath of
                Just _filePath -> do
                                    tell "Failed to parse configuration file: "
                                    tell _filePath
                                    tell "\n"
                                    tell "Example: \n"
                                    tell - showConfig defaultMoeConfig <> "\n"
                Nothing -> do
                            tell "The password argument '-k' is required.\n"
                            tell "Alternatively, use '-c' to provide a "
                            tell "configuration file.\n"

      throwError - _r ^. _Text 

    Just _config -> do
      let configStr = showConfig _config ^. _Text :: String
      io - debug_ - "Using config: " <> configStr
      pure - _config 
              

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

data AppType = TCP_App | UDP_App 
  deriving (Show, Eq)

moeApp:: MoeMonadT ()
moeApp = do
  _options <- ask 
  io - initLogger - _options ^. verbosity
  
  io - debug_ - show _options
  
  _config <- parseConfig - _options
  let _c = _config

  let _method = _config ^. method

  _cipherBox <- (io - initCipherBox _method (_config ^. password)) >>= \case
    Nothing -> throwError - "Invalid method '" 
                            <> _method ^. _Text
    Just (a, b, c, d) -> pure - CipherBox a b c d
  
  let _env = Env _options _config _cipherBox

  let localAppBuilder :: AppType 
                      -> String 
                      -> (ByteString -> (Socket, SockAddr) -> IO ()) 
                      -> (Socket, SockAddr) 
                      -> IO ()
      localAppBuilder aAppType aID aHandler s = 
        logSA "L loop" (pure s) - \(_localSocket, _localAddr) -> do
            
          setSocketOption _localSocket ReuseAddr 1
          bindSocket _localSocket _localAddr
          
          case aAppType of
            TCP_App -> do
              info_ - "LT: " <> aID <> " nyaa!"

              setSocket_TCP_FAST_OPEN _localSocket
              listen _localSocket maxListenQueue

              let handleLocal _socket = do
                    _s@(_newSocket, _newSockAddr) <- accept _socket
                    setSocketCloseOnExec _newSocket
                    setSocketSendFast _socket
                    
                    forkIO - catchExceptAsyncLog "LT" - 
                              logSA "L TCP client socket" (pure _s) -
                                aHandler ""

              forever - handleLocal _localSocket

            UDP_App -> do
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

  let localSocks5App :: (Socket, SockAddr) -> IO ()
      localSocks5App _s = localAppBuilder TCP_App 
                            ("Socks5 proxy " <> showWrapped (_s ^. _2))  
                            (local_Socks5_RequestHandler _env) - _s

      showForwarding :: Forward -> String
      showForwarding (Forward _localPort _remoteHost _remotePort) =
                          "["
                      <> show _localPort 
                      <> " -> " 
                      <> _remoteHost ^. _Text
                      <> ":"
                      <> show _remotePort
                      <> "]"


      forward_TCP_App :: Forward -> (Socket, SockAddr) -> IO ()
      forward_TCP_App _f _s = do
        let _m = showForwarding _f
        localAppBuilder TCP_App  ("TCP port forwarding " <> _m)
                                (local_TCP_ForwardRequestHandler _env _f) 
                                _s

      forward_UDP_App :: Forward -> (Socket, SockAddr) -> IO ()
      forward_UDP_App _f _s = do
        let _m = showForwarding _f 
        localAppBuilder UDP_App  ("UDP port forwarding " <> _m)
                                (local_UDP_ForwardRequestHandler _env _f) 
                                _s
      
  let remote_TCP_App :: (Socket, SockAddr) -> IO ()
      remote_TCP_App s = logSA "R loop" (pure s) -
        \(_remoteSocket, _remoteAddr) -> do
          info_ - "RT: TCP relay " <> showWrapped _remoteAddr <> " nyaa!"

          setSocketOption _remoteSocket ReuseAddr 1
          bindSocket _remoteSocket _remoteAddr

          setSocket_TCP_FAST_OPEN _remoteSocket
          
          listen _remoteSocket maxListenQueue

          let handleRemote _socket = do
                (_newSocket, _) <- accept _socket
                setSocketCloseOnExec _newSocket
                setSocketSendFast _socket
                
                forkIO - catchExceptAsyncLog "RT" - 
                            logSocket "R remote socket" (pure _newSocket) -
                              remote_TCP_RequestHandler _env 

          forever - handleRemote _remoteSocket

  let remote_UDP_App :: (Socket, SockAddr) -> IO ()
      remote_UDP_App s = logSA "R loop" (pure s) -
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


  let 
      remoteRun :: IO ()
      remoteRun = do
        let __TCP_App = foreverRun - catchExceptAsyncLog "R TCP app" - do
              getSocket (_c ^. remote) (_c ^. remotePort) Stream
                >>= remote_TCP_App 

        let __UDP_App = foreverRun - catchExceptAsyncLog "R UDP app" - do
              getSocket (_c ^. remote) (_c ^. remotePort) Datagram
                >>= remote_UDP_App 

        waitBoth __TCP_App __UDP_App

          
        
      localRun :: IO ()
      localRun = do
        let _forward_TCP_Apps = do
              forM_ (_options ^. forward_TCP) - \forwarding -> forkIO - do
                  foreverRun - catchExceptAsyncLog "L TCPForwarding app" - do
                    getSocket (_c ^. local) 
                      (forwarding ^. forwardLocalPort) 
                      Stream
                    >>= forward_TCP_App forwarding
          
        let _forward_UDP_Apps = do
              forM_ (_options ^. forward_UDP) - \forwarding -> forkIO - do
                  foreverRun - catchExceptAsyncLog "L UDPForwarding app" - do
                    getSocket (_c ^. local) 
                      (forwarding ^. forwardLocalPort) 
                      Datagram
                    >>= forward_UDP_App forwarding
        
        let _socks5App = foreverRun - catchExceptAsyncLog "L socks5 app" - do
              getSocket (_c ^. local) (_c ^. localPort) Stream
                >>= localSocks5App 

        _forward_TCP_Apps
        _forward_UDP_Apps
        if (_options ^. disableSocks5) 
          then 
            if (_options ^. forward_TCP & isn't _Empty)
                    || (_options ^. forward_UDP & isn't _Empty)
              then 
                forever - sleep 1000
              else
                error_ "Nothing to run!"
                
          else _socks5App

      debugRun :: IO ()
      debugRun = do
        catchExceptAsyncLog "Debug app" - do
          waitBothDebug
            (Just "localRun", localRun)
            (Just "remoteRun", remoteRun)

  io - case _options ^. runningMode of
    DebugMode -> debugRun
    RemoteMode -> remoteRun
    LocalMode -> localRun

