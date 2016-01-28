{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.TCP where

import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad.Writer hiding (listen)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.MoeSocks.BuilderAndParser
import Network.MoeSocks.Common
import Network.MoeSocks.Encrypt (identityCipher)
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
import Network.Socket hiding (send, recv, recvFrom, sendTo)
import Network.Socket.ByteString (recv)
import Prelude hiding ((-), take)
import qualified Data.ByteString as S
import qualified Data.Strict as S

_NoThrottle :: Maybe a
_NoThrottle = Nothing

local_SOCKS5_RequestHandler :: Env
                            -> Text
                            -> Int
                            -> ByteString
                            -> (Socket, SockAddr)
                            -> IO ()
local_SOCKS5_RequestHandler aEnv aRemoteHost aRemotePort _ (aSocket,_) = do
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

  local_TCP_RequestHandler  aEnv
                            aRemoteHost
                            aRemotePort
                            _parsedRequest
                            True
                            aSocket



local_TCP_ForwardRequestHandler :: Env
                                -> Text
                                -> Int
                                -> Forward
                                -> ByteString
                                -> (Socket, SockAddr)
                                -> IO ()
local_TCP_ForwardRequestHandler aEnv aRemoteHost aRemotePort aForwarding _
                                                              (aSocket,_) = do
  let _clientRequest = ClientRequest
                          TCP_IP_StreamConnection
                          (DomainName - aForwarding ^.
                            forwardTargetHost)
                          (aForwarding ^. forwardTargetPort)

  local_TCP_RequestHandler  aEnv
                            aRemoteHost
                            aRemotePort
                            (mempty, _clientRequest)
                            False
                            aSocket



local_TCP_RequestHandler :: Env
                          -> Text
                          -> Int
                          -> (ByteString, ClientRequest)
                          -> Bool
                          -> Socket
                          -> IO ()
local_TCP_RequestHandler aEnv
                        aRemoteHost
                        aRemotePort
                        (_partialBytesAfterClientRequest, _clientRequest)
                        shouldReplyClient aSocket = do
  let _addr = _clientRequest ^. addressType
      _IPLists = getIPLists aEnv

  debug_ - "checking: " <> show _addr
  withChecked_IP_List _addr _IPLists - do
    let
        _cipherBox = aEnv ^. cipherBox
        _obfuscation = aEnv ^. obfuscation
        _flushBound = aEnv ^. obfuscationFlushBound

        _initSocket =
            getSocket aRemoteHost aRemotePort Stream

    debug_ - "L: " <> show _clientRequest

    logSA "L remote socket" _initSocket -
      \(_remoteSocket, _remoteHost) -> do
      setSocketConfig aEnv _remoteSocket

      _remoteSocketName <- getSocketName _remoteSocket

      when shouldReplyClient - do
        let _connectionReplyBuilder = connectionReplyBuilder _remoteSocketName
        send_ aSocket - builder_To_ByteString _connectionReplyBuilder


      _localPeerAddr <- getPeerName aSocket
      let _msg = showRelay _localPeerAddr _clientRequest

      info_ - "LT: " <> _msg

      let handleLocal _remoteSocket = do
            _encodeIV <- _cipherBox ^. generate_IV
            _encrypt <- _cipherBox ^. encryptBuilder - _encodeIV

            let
                _header = shadowSocksRequestBuilder _clientRequest

            _sendChannel <- newTBQueueIO - aEnv ^. tcpBufferSize
            _receiveChannel <- newTBQueueIO - aEnv ^. tcpBufferSize

            let info_Id x = x <> " " <> _msg
                _timeout = aEnv ^. timeout * 1000 * 1000
                _throttle =
                  if aEnv ^. throttle
                    then Just - aEnv ^. throttleSpeed
                    else Nothing

            _eHeader <- _encrypt - S.Just - builder_To_ByteString _header
            _ePartial <- _encrypt - S.Just _partialBytesAfterClientRequest
            let _padding = S.length (_eHeader <> _ePartial)

            _eInit <- _encrypt . S.Just =<< recv aSocket (4096 + (-_padding))

            let _initBytes = _encodeIV <> _eHeader <> _ePartial <> _eInit

            if aEnv ^. fastOpen
              then
                sendFast _remoteSocket _initBytes _remoteHost
              else do
                connect _remoteSocket _remoteHost
                send_ _remoteSocket _initBytes

            let sendThread = do
                  let _produce = do
                                    produceLoop (info_Id "L --> + Loop")
                                      _timeout
                                      _NoThrottle
                                      aSocket
                                      _sendChannel
                                      _encrypt

                  let _consume = do
                                    consumeLoop (info_Id "L --> - Loop")
                                      _timeout
                                      _throttle
                                      _remoteSocket
                                      _sendChannel
                                      _obfuscation
                                      _flushBound
                  finally
                    (
                      connectMarket (Just - info_Id "L --> +", _produce)
                                    (Just - info_Id "L --> -", _consume)
                    ) -
                    pure ()


            let receiveThread = do
                  _decodeIV <- recv _remoteSocket (_cipherBox ^. ivLength)
                  _decrypt <- _cipherBox ^. decryptBuilder - _decodeIV

                  let _produce = produceLoop (info_Id "L <-- + Loop")
                                    _timeout
                                    _NoThrottle
                                    _remoteSocket
                                    _receiveChannel
                                    _decrypt

                  let _consume = do
                                    consumeLoop (info_Id "L <-- - Loop")
                                      _timeout
                                      _NoThrottle
                                      aSocket
                                      _receiveChannel
                                      False
                                      _flushBound
                  finally
                    (
                      connectMarket (Just - info_Id "L <-- +", _produce)
                                    (Just - info_Id "L <-- -", _consume)
                    ) -
                    pure ()

            connectTunnel
              (Just - info_Id "L -->", sendThread)
              (Just - info_Id "L <--", receiveThread)


      handleLocal _remoteSocket


remote_TCP_RequestHandler :: Env -> Socket -> IO ()
remote_TCP_RequestHandler aEnv aSocket = do
  let
      _obfuscation = aEnv ^. obfuscation
      _cipherBox = aEnv ^. cipherBox
      _flushBound = aEnv ^. obfuscationFlushBound

  _decodeIV <- recv aSocket (_cipherBox ^. ivLength)
  _decrypt <- _cipherBox ^. decryptBuilder - _decodeIV

  (_partialBytesAfterRequest, _clientRequest) <- parseSocket
                                          "clientRequest"
                                          mempty
                                          _decrypt
                                          (shadowSocksRequestParser
                                            TCP_IP_StreamConnection)
                                          aSocket

  logSA "R target socket" (initTarget _clientRequest) - \_r -> do
    let (_targetSocket, _targetHost) = _r
        (_addr, _) = sockAddr_To_Pair _targetHost
        _IPLists = getIPLists aEnv

    debug_ - "checking: " <> show _addr
    withChecked_IP_List _addr _IPLists - do
      setSocketConfig aEnv _targetSocket

      _remotePeerAddr <- getPeerName aSocket
      let _msg = showRelay _remotePeerAddr _clientRequest

      info_ - "RT: " <> _msg

      let _initBytes = _partialBytesAfterRequest

      if aEnv ^. fastOpen
        then
          sendFast _targetSocket _initBytes _targetHost
        else do
          connect _targetSocket _targetHost
          send_ _targetSocket _initBytes

      let
          handleTarget __targetSocket = do
            _sendChannel <- newTBQueueIO - aEnv ^. tcpBufferSize
            _receiveChannel <- newTBQueueIO - aEnv ^. tcpBufferSize

            let info_Id x = x <> " " <> _msg
                -- let remote wait slightly longer, so local can timeout
                -- and disconnect
                _timeout = (aEnv ^. timeout + 30) * 1000 * 1000

                _throttle =
                  if aEnv ^. throttle
                    then Just - aEnv ^. throttleSpeed
                    else Nothing

            let sendThread = do
                  let _produce = do
                                    produceLoop (info_Id "R --> + Loop")
                                      _timeout
                                      _NoThrottle
                                      aSocket
                                      _sendChannel
                                      _decrypt

                  let _consume = consumeLoop (info_Id "R --> - Loop")
                                    _timeout
                                    _NoThrottle
                                    _targetSocket
                                    _sendChannel
                                    False
                                    _flushBound

                  finally
                    (
                      connectMarket (Just - info_Id "R --> +", _produce)
                                    (Just - info_Id "R --> -", _consume)
                    ) -
                    pure ()

            let receiveThread = do
                  _encodeIV <- _cipherBox ^. generate_IV
                  _encrypt <- _cipherBox ^. encryptBuilder - _encodeIV
                  sendBytes _receiveChannel _encodeIV

                  let _produce = do
                                    produceLoop (info_Id "R <-- + Loop")
                                      _timeout
                                      _NoThrottle
                                      _targetSocket
                                      _receiveChannel
                                      _encrypt


                  let _consume = do
                                    consumeLoop (info_Id "R <-- - Loop")
                                      _timeout
                                      _throttle
                                      aSocket
                                      _receiveChannel
                                      _obfuscation
                                      _flushBound

                  finally
                    (
                      connectMarket (Just - info_Id "R <-- +", _produce)
                                    (Just - info_Id "R <-- -", _consume)
                    ) -
                    pure ()

            connectTunnel
              (Just - info_Id "R -->", sendThread)
              (Just - info_Id "R <--", receiveThread)

      handleTarget _targetSocket
