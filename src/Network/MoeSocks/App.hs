{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.MoeSocks.App where

import Control.Concurrent
import Control.Concurrent.Async hiding (waitBoth)
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader hiding (localAddress)
import Control.Monad.Writer hiding (listen)
import Data.Aeson hiding (Result)
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lens
import Network.MoeSocks.Config
import Network.MoeSocks.Constant
import Network.MoeSocks.Encrypt (initCipherBox, safeMethods, unsafeMethods)
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

parseConfig :: Options -> MoeMonadT Config
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
                ("server", "remoteAddress")
              , ("server_port", "remotePort")
              , ("local_address", "localAddress")
              ]

        in
        fixes ^? ix x & fromMaybe x

      toParsableConfig :: Value -> Value
      toParsableConfig = asList - each . _1 %~  (
                                                  T.cons '_' 
                                                . toHaskellNamingConvention
                                                . fromShadowSocksConfig
                                                )  

      toReadableConfig :: Value -> Value
      toReadableConfig = asList - each . _1 %~ T.tail 

      showConfig :: Config -> Text
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

      optionalConfig = filterEssentialConfig - toJSON defaultConfig
      
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
                                    tell - showConfig defaultConfig <> "\n"
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

data RelayType = TCP_Relay | UDP_Relay 
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


  let localRelay :: RelayType 
                      -> String 
                      -> (ByteString -> (Socket, SockAddr) -> IO ()) 
                      -> (Socket, SockAddr) 
                      -> IO ()
      localRelay aRelayType aID aHandler s = 
        logSA "L loop" (pure s) - \(_localSocket, _localAddr) -> do
            
          setSocketOption _localSocket ReuseAddr 1
          bindSocket _localSocket _localAddr
          
          case aRelayType of
            TCP_Relay -> do
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

            UDP_Relay -> do
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
      local_SOCKS5 _s = localRelay TCP_Relay 
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
        localRelay TCP_Relay  ("TCP port forwarding " <> _m)
                                (local_TCP_ForwardRequestHandler _env _f) 
                                _s

      localForward_UDP :: Forward -> (Socket, SockAddr) -> IO ()
      localForward_UDP _f _s = do
        let _m = showForwarding _f 
        localRelay UDP_Relay  ("UDP port forwarding " <> _m)
                                (local_UDP_ForwardRequestHandler _env _f) 
                                _s
      
  let remote_TCP_Relay :: (Socket, SockAddr) -> IO ()
      remote_TCP_Relay s = logSA "R loop" (pure s) -
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


  let 
      runRemote :: IO ()
      runRemote = do
        let config_To_Runtime :: Config -> Runtime -> Runtime
            config_To_Runtime _config _runtime = 
              let _remote_TCP_Relay =   
                      RemoteRelay
                        Remote_TCP_Relay
                        (_config ^. remoteAddress)
                        (_config ^. remotePort)

                  _remote_UDP_Relay = 
                      RemoteRelay
                        Remote_UDP_Relay
                        (_config ^. remoteAddress)
                        (_config ^. remotePort)
              in
              _runtime & remoteRelays .~ [_remote_TCP_Relay, _remote_UDP_Relay]

        let buildRemoteRelay :: RemoteRelay -> IO (Async ())
            buildRemoteRelay _remoteRelay = do
              let _address = _remoteRelay ^. remoteRelayAddress
                  _port = _remoteRelay ^. remoteRelayPort
              
              async - foreverRun - do
                case _remoteRelay ^. remoteRelayType of
                  Remote_TCP_Relay -> catchExceptAsyncLog "R TCP Relay" - do
                                        getSocket _address _port Stream
                                          >>= remote_TCP_Relay 

                  Remote_UDP_Relay  -> catchExceptAsyncLog "R UDP Relay" - do
                                        getSocket _address _port Datagram
                                          >>= remote_UDP_Relay 
                
        relays <- mapM buildRemoteRelay - 
                        config_To_Runtime _c mempty ^. remoteRelays

        waitAnyCancel relays

        pure ()

          
        
      runLocal :: IO ()
      runLocal = do
        let _localForward_TCPs = do
              forM_ (_options ^. forward_TCPs) - \forwarding -> forkIO - do
                  foreverRun - catchExceptAsyncLog "L TCP_Forwarding" - do
                    getSocket (_c ^. localAddress) 
                      (forwarding ^. forwardLocalPort) 
                      Stream
                    >>= localForward_TCP forwarding
          
        let _localForward_UDPs = do
              forM_ (_options ^. forward_UDPs) - \forwarding -> forkIO - do
                  foreverRun - catchExceptAsyncLog "L UDP_Forwarding" - do
                    getSocket (_c ^. localAddress) 
                      (forwarding ^. forwardLocalPort) 
                      Datagram
                    >>= localForward_UDP forwarding
        
        let _local_SOCKS5 = foreverRun - catchExceptAsyncLog "L SOCKS5" - do
              getSocket (_c ^. localAddress) (_c ^. localPort) Stream
                >>= local_SOCKS5 

        _localForward_TCPs
        _localForward_UDPs
        if (_options ^. disable_SOCKS5) 
          then 
            if (_options ^. forward_TCPs & isn't _Empty)
                    || (_options ^. forward_UDPs & isn't _Empty)
              then 
                forever - sleep 1000
              else
                error_ "Nothing to run!"
                
          else _local_SOCKS5

      runDebug :: IO ()
      runDebug = do
        catchExceptAsyncLog "runDebug" - do
          waitBothDebug
            (Just "runLocal", runLocal)
            (Just "runRemote", runRemote)

  io - case _options ^. runningMode of
    DebugMode -> runDebug
    RemoteMode -> runRemote
    LocalMode -> runLocal

