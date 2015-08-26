{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.TCP where

import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Writer hiding (listen)
import Data.ByteString (ByteString)
import Data.Text.Lens
import Network.MoeSocks.BuilderAndParser
import Network.MoeSocks.Common
import Network.MoeSocks.Constant
import Network.MoeSocks.Encrypt (identityCipher)
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
import Network.Socket hiding (send, recv, recvFrom, sendTo)
import Network.Socket.ByteString (recv)
import Prelude hiding ((-), take)
import qualified Data.Strict as S

local_Socks5_RequestHandler :: Env
                            -> ByteString 
                            -> (Socket, SockAddr) 
                            -> IO ()
local_Socks5_RequestHandler aEnv _ (aSocket,_) = do
  (_partialBytesAfterGreeting, _r) <- 
      parseSocket "clientGreeting" mempty identityCipher
        greetingParser aSocket

  when (not - _No_authentication `elem` (_r ^. authenticationMethods)) - 
    throwIO - ParseException
               "Client does not support no authentication method"

  send_ aSocket - builder_To_ByteString greetingReplyBuilder 

  _parsedRequest <- parseSocket 
                                "clientRequest" 
                                _partialBytesAfterGreeting
                                identityCipher
                                connectionParser
                                aSocket

  local_TCP_RequestHandler aEnv _parsedRequest True aSocket



local_TCP_ForwardRequestHandler :: Env
                                -> Forward 
                                -> ByteString 
                                -> (Socket, SockAddr) 
                                -> IO ()
local_TCP_ForwardRequestHandler aEnv aForwarding _ (aSocket,_) = do
  let _clientRequest = ClientRequest
                          TCP_IP_stream_connection
                          (Domain_name - aForwarding ^. 
                            forwardRemoteHost)
                          (aForwarding ^. forwardRemotePort)
              
  local_TCP_RequestHandler aEnv
                          (mempty, _clientRequest) False aSocket



local_TCP_RequestHandler :: Env
                          -> (ByteString, ClientRequest) 
                          -> Bool 
                          -> Socket 
                          -> IO ()
local_TCP_RequestHandler aEnv
                        (_partialBytesAfterClientRequest, _clientRequest) 
                        shouldReplyClient aSocket = do
  let 
      _c = aEnv ^. config 
      _cipherBox = aEnv ^. cipherBox
      _obfuscation = aEnv ^. options . obfuscation
      _flushBound = _c ^. obfuscationFlushBound

      _initSocket = 
          getSocket (_c ^. remote) (_c ^. remotePort) Stream 

  puts - "L: " <> show _clientRequest
  
  logSA "L remote socket" _initSocket - 
    \(_remoteSocket, _remoteAddress) -> do
    connect _remoteSocket _remoteAddress


    _localPeerAddr <- getPeerName aSocket
    _remoteSocketName <- getSocketName _remoteSocket
    
    when shouldReplyClient - do
      let _connectionReplyBuilder = connectionReplyBuilder _remoteSocketName
      send_ aSocket - builder_To_ByteString _connectionReplyBuilder
    

    let _msg = show _localPeerAddr <> " -> " <> showRequest _clientRequest
    
    _log - "L T: " <> _msg

    let handleLocal _remoteSocket = do
          _encodeIV <- _cipherBox ^. generate_IV 
          _encrypt <- _cipherBox ^. encryptBuilder - _encodeIV
          
          let 
              _header = shadowSocksRequestBuilder _clientRequest
          
          _sendChannel <- newTBQueueIO - _c ^. tcpBufferSize
          _receiveChannel <- newTBQueueIO - _c ^. tcpBufferSize

          let _logId x = x <> " " <> _msg
              _timeout = _c ^. timeout * 1000 * 1000
              _throttle = 
                if _c ^. throttle
                  then Just - _c ^. throttleSpeed
                  else Nothing

          let sendThread = do
                sendBytes _sendChannel _encodeIV
                sendBuilderEncrypt _sendChannel _encrypt _header

                when (_partialBytesAfterClientRequest & isn't _Empty) -
                  sendBytesEncrypt _sendChannel _encrypt 
                    _partialBytesAfterClientRequest

                let _produce = do
                                  produceLoop (_logId "L --> + Loop")
                                    _timeout
                                    _NoThrottle
                                    aSocket 
                                    _sendChannel 
                                    _encrypt

                let _consume = do
                                  consumeLoop (_logId "L --> - Loop")
                                    _timeout
                                    _throttle
                                    _remoteSocket 
                                    _sendChannel
                                    _obfuscation
                                    _flushBound
                finally
                  (
                    connectMarket (Just - _logId "L --> +", _produce)
                                  (Just - _logId "L --> -", _consume)
                  ) -
                  pure ()

          let receiveThread = do
                _decodeIV <- recv _remoteSocket (_cipherBox ^. ivLength)
                _decrypt <- _cipherBox ^. decryptBuilder - _decodeIV

                let _produce = produceLoop (_logId "L <-- + Loop")
                                  _timeout
                                  _NoThrottle
                                  _remoteSocket 
                                  _receiveChannel
                                  _decrypt

                let _consume = do
                                  consumeLoop (_logId "L <-- - Loop")
                                    _timeout
                                    _NoThrottle
                                    aSocket 
                                    _receiveChannel
                                    False
                                    _flushBound
                finally 
                  (
                    connectMarket (Just - _logId "L <-- +", _produce)
                                  (Just - _logId "L <-- -", _consume)
                  ) -
                  pure ()

          connectTunnel
            (Just - _logId "L -->", sendThread)
            (Just - _logId "L <--", receiveThread)


    handleLocal _remoteSocket


remote_TCP_RequestHandler :: Env -> Socket -> IO ()
remote_TCP_RequestHandler aEnv aSocket = do
  let
      _obfuscation = aEnv ^. options . obfuscation
      _cipherBox = aEnv ^. cipherBox
      _c = aEnv ^. config
      _options = aEnv ^. options
      _flushBound = _c ^. obfuscationFlushBound

  _decodeIV <- recv aSocket (_cipherBox ^. ivLength)
  _decrypt <- _cipherBox ^. decryptBuilder - _decodeIV

  (_leftOverBytes, _clientRequest) <- parseSocket 
                                          "clientRequest"
                                          mempty
                                          _decrypt
                                          (shadowSocksRequestParser 
                                            TCP_IP_stream_connection)
                                          aSocket
  
  logSA "R target socket" (initTarget _clientRequest) - \_r -> do
    let (_targetSocket, _targetSocketAddress) = _r 
        (_addr, _) = sockAddr_To_Pair _targetSocketAddress
        _forbidden_IP = _options ^. forbidden_IP

    puts - "checking: " <> show _addr <> " ? " <> show _forbidden_IP
    if checkForbidden_IP_List _addr _forbidden_IP
      then pute - showAddressType _addr ^. _Text 
                  <> " is in forbidden-ip list"
      else do
      connect _targetSocket _targetSocketAddress

      _remotePeerAddr <- getPeerName aSocket
      _targetPeerAddr <- getPeerName _targetSocket

      let _msg = show _remotePeerAddr <> " -> " <> showRequest _clientRequest

      _log - "R T: " <> _msg

      let 
          handleTarget _leftOverBytes __targetSocket = do
            _sendChannel <- newTBQueueIO - _c ^. tcpBufferSize
            _receiveChannel <- newTBQueueIO - _c ^. tcpBufferSize

            let _logId x = x <> " " <> _msg
                -- let remote wait slightly longer, so local can timeout
                -- and disconnect
                _timeout = (_c ^. timeout + 30) * 1000 * 1000
                
                _throttle = 
                  if _c ^. throttle
                    then Just - _c ^. throttleSpeed
                    else Nothing

            let sendThread = do
                  when (_leftOverBytes & isn't _Empty) -
                    atomically - writeTBQueue _sendChannel - 
                                  S.Just _leftOverBytes

                  let _produce = do
                                    produceLoop (_logId "R --> + Loop")
                                      _timeout
                                      _NoThrottle 
                                      aSocket
                                      _sendChannel
                                      _decrypt

                  let _consume = consumeLoop (_logId "R --> - Loop")
                                    _timeout
                                    _NoThrottle
                                    _targetSocket
                                    _sendChannel
                                    False
                                    _flushBound

                  finally
                    (
                      connectMarket (Just - _logId "R --> +", _produce)
                                    (Just - _logId "R --> -", _consume)
                    ) -
                    pure ()

            let receiveThread = do
                  _encodeIV <- _cipherBox ^. generate_IV 
                  _encrypt <- _cipherBox ^. encryptBuilder - _encodeIV
                  sendBytes _receiveChannel _encodeIV

                  let _produce = do
                                    produceLoop (_logId "R <-- + Loop")
                                      _timeout
                                      _NoThrottle 
                                      _targetSocket
                                      _receiveChannel
                                      _encrypt


                  let _consume = do
                                    consumeLoop (_logId "R <-- - Loop")
                                      _timeout
                                      _throttle
                                      aSocket
                                      _receiveChannel
                                      _obfuscation
                                      _flushBound

                  finally 
                    (
                      connectMarket (Just - _logId "R <-- +", _produce)
                                    (Just - _logId "R <-- -", _consume)
                    ) -
                    pure ()

            connectTunnel
              (Just - _logId "R -->", sendThread)
              (Just - _logId "R <--", receiveThread)
            
      handleTarget _leftOverBytes _targetSocket
