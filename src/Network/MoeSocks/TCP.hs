{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.TCP where

import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Writer hiding (listen)
import Data.ByteString (ByteString)
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

local_Socks5_RequestHandler :: CipherBox 
                            -> MoeConfig 
                            -> ByteString 
                            -> (Socket, SockAddr) 
                            -> IO ()
local_Socks5_RequestHandler aCipherBox aConfig _ (aSocket,_) = do
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

  local_TCP_RequestHandler aCipherBox aConfig _parsedRequest True aSocket



local_TCP_ForwardRequestHandler :: CipherBox 
                                -> MoeConfig 
                                -> Forward 
                                -> ByteString 
                                -> (Socket, SockAddr) 
                                -> IO ()
local_TCP_ForwardRequestHandler aCipherBox aConfig aForwarding _ (aSocket,_) = do
  let _clientRequest = ClientRequest
                          TCP_IP_stream_connection
                          (Domain_name - aForwarding ^. 
                            forwardRemoteHost)
                          (aForwarding ^. forwardRemotePort)
              
  local_TCP_RequestHandler aCipherBox aConfig (mempty, _clientRequest) False aSocket



local_TCP_RequestHandler :: CipherBox 
                          -> MoeConfig 
                          -> (ByteString, ClientRequest) 
                          -> Bool 
                          -> Socket 
                          -> IO ()
local_TCP_RequestHandler aCipherBox aConfig 
                        (_partialBytesAfterClientRequest, _clientRequest) 
                        shouldReplyClient aSocket = do
  let 
      _c = aConfig 
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

    let handleLocal __remoteSocket = do
          _encodeIV <- aCipherBox ^. generateIV 
          _encrypt <- aCipherBox ^. encryptBuilder - _encodeIV
          
          let 
              _header = shadowSocksRequestBuilder _clientRequest
          
          sendChannel <- newTBQueueIO - aConfig ^. tcpBufferSize
          receiveChannel <- newTBQueueIO - aConfig ^. tcpBufferSize

          let _logId x = x <> " " <> _msg
              _timeout = aConfig ^. timeout * 1000 * 1000
              _throttle = 
                if aConfig ^. throttle
                  then Just - aConfig ^. throttleSpeed
                  else Nothing

          let sendThread = do
                sendBytes sendChannel _encodeIV
                sendBuilderEncrypt sendChannel _encrypt _header

                when (_partialBytesAfterClientRequest & isn't _Empty) -
                  sendBytesEncrypt sendChannel _encrypt 
                    _partialBytesAfterClientRequest

                let _produce = do
                                  produceLoop (_logId "L --> + Loop")
                                    _timeout
                                    _NoThrottle
                                    aSocket 
                                    sendChannel 
                                    _encrypt

                let _consume = do
                                  consumeLoop (_logId "L --> - Loop")
                                    _timeout
                                    _throttle
                                    __remoteSocket 
                                    sendChannel
                finally
                  (
                    connectMarket (Just - _logId "L --> +", _produce)
                                  (Just - _logId "L --> -", _consume)
                  ) -
                  pure ()

          let receiveThread = do
                _decodeIV <- recv __remoteSocket (aCipherBox ^. ivLength)
                _decrypt <- aCipherBox ^. decryptBuilder - _decodeIV

                let _produce = produceLoop (_logId "L <-- + Loop")
                                  _timeout
                                  _NoThrottle
                                  __remoteSocket 
                                  receiveChannel
                                  _decrypt

                let _consume = do
                                  consumeLoop (_logId "L <-- - Loop")
                                    _timeout
                                    _NoThrottle
                                    aSocket 
                                    receiveChannel
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


remote_TCP_RequestHandler :: CipherBox -> MoeConfig -> Socket -> IO ()
remote_TCP_RequestHandler aCipherBox aConfig aSocket = do
  _decodeIV <- recv aSocket (aCipherBox ^. ivLength)
  _decrypt <- aCipherBox ^. decryptBuilder - _decodeIV

  (_leftOverBytes, _clientRequest) <- parseSocket 
                                          "clientRequest"
                                          mempty
                                          _decrypt
                                          (shadowSocksRequestParser 
                                            TCP_IP_stream_connection)
                                          aSocket
                                          

  {-puts - "Remote get: " <> show _clientRequest-}
  
  logSA "R target socket" (initTarget _clientRequest) - \_r -> do
    let (_targetSocket, _targetSocketAddress) = _r 

    connect _targetSocket _targetSocketAddress

    _remotePeerAddr <- getPeerName aSocket
    _targetPeerAddr <- getPeerName _targetSocket

    let _msg = show _remotePeerAddr <> " -> " <> showRequest _clientRequest

    _log - "R T: " <> _msg

    let 
        handleTarget __leftOverBytes __targetSocket = do
          sendChannel <- newTBQueueIO - aConfig ^. tcpBufferSize
          receiveChannel <- newTBQueueIO - aConfig ^. tcpBufferSize

          let _logId x = x <> " " <> _msg
              _timeout = aConfig ^. timeout * 1000 * 1000
              
              _throttle = 
                if aConfig ^. throttle
                  then Just - aConfig ^. throttleSpeed
                  else Nothing

          let sendThread = do
                when (_leftOverBytes & isn't _Empty) -
                  atomically - writeTBQueue sendChannel - S.Just _leftOverBytes

                let _produce = do
                                  produceLoop (_logId "R --> + Loop")
                                    _timeout
                                    _NoThrottle 
                                    aSocket
                                    sendChannel
                                    _decrypt

                let _consume = consumeLoop (_logId "R --> - Loop")
                                  _timeout
                                  _NoThrottle
                                  __targetSocket
                                  sendChannel

                finally
                  (
                    connectMarket (Just - _logId "R --> +", _produce)
                                  (Just - _logId "R --> -", _consume)
                  ) -
                  pure ()

          let receiveThread = do
                _encodeIV <- aCipherBox ^. generateIV 
                _encrypt <- aCipherBox ^. encryptBuilder - _encodeIV
                sendBytes receiveChannel _encodeIV

                let _produce = do
                                  produceLoop (_logId "R <-- + Loop")
                                    _timeout
                                    _NoThrottle 
                                    __targetSocket
                                    receiveChannel
                                    _encrypt


                let _consume = do
                                  consumeLoop (_logId "R <-- - Loop")
                                    _timeout
                                    _throttle
                                    aSocket
                                    receiveChannel

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
