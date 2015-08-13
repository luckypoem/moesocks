{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Network.MoeSocks.TCP where

import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Writer hiding (listen)
import Data.ByteString (ByteString)
import Network.MoeSocks.BuilderAndParser
import Network.MoeSocks.Helper
import Network.MoeSocks.Common
import Network.MoeSocks.Type
import Network.Socket hiding (send, recv, recvFrom, sendTo)
import Prelude hiding ((-), take)
import qualified Data.List as L


forwardTCPRequestHandler :: MoeConfig -> Forward -> 
                                  ByteString -> (Socket, SockAddr) -> IO ()
forwardTCPRequestHandler aConfig aTCPForwarding _ (aSocket,_) = do
  let _clientRequest = ClientRequest
                          TCP_IP_stream_connection
                          (Domain_name - aTCPForwarding ^. 
                            forwardRemoteHost)
                          (aTCPForwarding ^. forwardRemotePort)
              
  localTCPRequestHandler aConfig (_clientRequest, mempty) False aSocket



localTCPRequestHandler :: MoeConfig -> (ClientRequest, ByteString) -> 
                        Bool -> Socket -> IO ()
localTCPRequestHandler aConfig (_clientRequest, _partialBytesAfterClientRequest) 
                    shouldReplyClient aSocket = do
  let 
      _c = aConfig 
      _initSocket = 
          getSocket (_c ^. remote) (_c ^. remotePort) Stream 

  puts - "L : " <> show _clientRequest
  
  logSA "L remote socket" _initSocket - 
    \(_remoteSocket, _remoteAddress) -> do
    connect _remoteSocket _remoteAddress


    _localPeerAddr <- getPeerName aSocket
    _remoteSocketName <- getSocketName _remoteSocket
    
    when shouldReplyClient - do
      let _connectionReplyBuilder = connectionReplyBuilder _remoteSocketName
      send_ aSocket - builder_To_ByteString _connectionReplyBuilder
    

    let _msg = 
                concat - L.intersperse " -> " 
                [ 
                  show _localPeerAddr
                , showRequest _clientRequest
                ]
    
    _log - "L " -- <> showConnectionType (_clientRequest ^. connectionType)
                <> ": " <> _msg

    let handleLocal __remoteSocket = do
          (_encrypt, _decrypt) <- getCipher
                                    (aConfig ^. method)
                                    (aConfig ^. password)


          let 
              _header = shadowSocksRequestBuilder _clientRequest
          
          sendChannel <- newTBQueueIO - aConfig ^. tcpBufferSizeInPacket
          receiveChannel <- newTBQueueIO - aConfig ^. tcpBufferSizeInPacket

          let _logId x = x <> " " <> _msg
              _timeout = aConfig ^. timeout * 1000 * 1000
              _throttle = 
                if aConfig ^. throttle
                  then Just - aConfig ^. throttleSpeed
                  else Nothing

          let sendThread = do
                sendBuilderEncrypted 
                  sendChannel _encrypt _header

                when (_partialBytesAfterClientRequest & isn't _Empty) -
                  atomically . writeTBQueue sendChannel . Just =<< 
                    _encrypt _partialBytesAfterClientRequest


                let _produce = do
                                  produceLoop (_logId "L --> + Loop")
                                    _timeout
                                    Nothing
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
                let _produce = produceLoop (_logId "L <-- + Loop")
                                  _timeout
                                  Nothing
                                  __remoteSocket 
                                  receiveChannel
                                  _decrypt

                let _consume = do
                                  consumeLoop (_logId "L <-- - Loop")
                                    _timeout
                                    Nothing
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


remoteTCPRequestHandler:: MoeConfig -> Socket -> IO ()
remoteTCPRequestHandler aConfig aSocket = do
  (_encrypt, _decrypt) <- getCipher
                            (aConfig ^. method)
                            (aConfig ^. password)
  
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

    let _msg = 
                concat - L.intersperse " -> " - 
                [ 
                  show _remotePeerAddr
                , showRequest _clientRequest
                ]

    _log - "R " -- <> showConnectionType (_clientRequest ^. connectionType)
                  <> ": " <> _msg

    let 
        handleTarget __leftOverBytes __targetSocket = do
          sendChannel <- newTBQueueIO - aConfig ^. tcpBufferSizeInPacket
          receiveChannel <- newTBQueueIO - aConfig ^. tcpBufferSizeInPacket

          let _logId x = x <> " " <> _msg
              _timeout = aConfig ^. timeout * 1000 * 1000
              
              _throttle = 
                if aConfig ^. throttle
                  then Just - aConfig ^. throttleSpeed
                  else Nothing

          let sendThread = do
                when (_leftOverBytes & isn't _Empty) -
                  atomically - writeTBQueue sendChannel - Just _leftOverBytes

                let _produce = do
                                  produceLoop (_logId "R --> + Loop")
                                    _timeout
                                    Nothing
                                    aSocket
                                    sendChannel
                                    _decrypt

                let _consume = consumeLoop (_logId "R --> - Loop")
                                  _timeout
                                  Nothing
                                  __targetSocket
                                  sendChannel

                finally
                  (
                    connectMarket (Just - _logId "R --> +", _produce)
                                      (Just - _logId "R --> -", _consume)
                  ) -
                  pure ()

          let receiveThread = do
                let _produce = do
                                  produceLoop (_logId "R --> + Loop")
                                    _timeout
                                    Nothing
                                    __targetSocket
                                    receiveChannel
                                    _encrypt


                let _consume = do
                                  consumeLoop (_logId "R --> - Loop")
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
