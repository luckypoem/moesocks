{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.UDP where

import           Control.Exception (throwIO)
import           Control.Lens
import           Control.Monad (when)
import           Data.Monoid ((<>))
import           Data.Attoparsec.ByteString (IResult(Done), parse)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.Strict as S
import           Data.Text (Text)
import           Network.Socket (Socket, SockAddr)
import           Network.Socket (connect, SocketType(Datagram))
import           Network.Socket.ByteString (sendAllTo)

import           Network.MoeSocks.BuilderAndParser (shadowSocksRequestParser)
import           Network.MoeSocks.BuilderAndParser (shadowSocksRequestBuilder)
import           Network.MoeSocks.BuilderAndParser (sockAddr_To_Pair)
import           Network.MoeSocks.Common (withChecked_IP_List, showRelay, initTarget, getIPLists)
import           Network.MoeSocks.Type

import           Network.MoeSocks.Helper ((-), recv_UDP, info_, debug_, send_)
import           Network.MoeSocks.Helper (logSA, getSocket, ParseException(ParseException))
import           Network.MoeSocks.Helper (builder_To_ByteString)
import           Prelude hiding ((-), take)


buildShadowSocksRequest :: ClientRequest -> ByteString -> ByteString
buildShadowSocksRequest aClientRequest aMessage =
  let _header = shadowSocksRequestBuilder aClientRequest
  in
  builder_To_ByteString _header <> aMessage

parseShadowSocksRequest :: ByteString -> IO (ByteString, ClientRequest)
parseShadowSocksRequest aMessage =
  case parse (shadowSocksRequestParser UDP_Port) aMessage of
    Done _i _r -> pure (_i, _r)
    _ -> throwIO - ParseException -
            "R Failed to parse UDP request"


processAll :: Cipher -> ByteString -> IO ByteString
processAll f x =
  (<>) <$> f (S.Just x) <*> f S.Nothing

local_UDP_ForwardRequestHandler :: Env
                                -> Text
                                -> Port
                                -> Forward
                                -> ByteString
                                -> (Socket,SockAddr)
                                -> IO ()
local_UDP_ForwardRequestHandler aEnv
                                aRemoteHost
                                aRemotePort
                                aForwarding
                                aMessage
                                (aSocket, aSockAddr) = do

  let
      _cipherBox = aEnv ^. cipherBox

  let _clientRequest = ClientRequest
                          UDP_Port
                          (DomainName - aForwarding ^.
                            forwardTargetHost)
                          (aForwarding ^. forwardTargetPort)

  {-debug_ - "L UDP: " <> show _clientRequest-}

  let _addr = _clientRequest ^. addressType
      _IPLists = getIPLists aEnv

  debug_ - "checking: " <> show _addr

  withChecked_IP_List _addr _IPLists - do
    _sa <- getSocket aRemoteHost aRemotePort Datagram

    logSA "L UDP -->:" (pure _sa) -
      \(_remoteSocket, _remoteAddr) -> do
        connect _remoteSocket _remoteAddr

        _encodeIV <- _cipherBox ^. generate_IV

        _encrypt <- _cipherBox ^. encryptBuilder - _encodeIV

        {-let (_encrypt, _decrypt) = (pure, pure)-}

        let _bytes = buildShadowSocksRequest _clientRequest aMessage

        {-debug_ - "L UDP sending: " <> show _bytes-}

        let _msg = showRelay aSockAddr _clientRequest
        info_ - "LU: " <> _msg

        _eMsg <- _encrypt (S.Just _bytes)

        send_ _remoteSocket - _encodeIV <> _eMsg

        _response <- recv_UDP _remoteSocket

        let (_decodeIV, _responseMsg) = S.splitAt (_cipherBox ^. ivLength)
                                          _response
        _decrypt <- _cipherBox ^. decryptBuilder - _decodeIV

        (_r, _) <- processAll _decrypt _responseMsg
                                        >>= parseShadowSocksRequest

        when (_r & isn't _Empty) - do
          sendAllTo aSocket _r aSockAddr


remote_UDP_RequestHandler :: Env
                          -> ByteString
                          -> (Socket, SockAddr)
                          -> IO ()
remote_UDP_RequestHandler aEnv
                          aMessage
                          (aSocket, aSockAddr) = do
  let _cipherBox = aEnv ^. cipherBox

  let (_decodeIV, _eMsg) = S.splitAt (_cipherBox ^. ivLength)
                                       aMessage

  _decrypt <- _cipherBox ^. decryptBuilder - _decodeIV
  _msg <- processAll _decrypt _eMsg

  (_decryptedMessage, _clientRequest) <- parseShadowSocksRequest _msg

  {-debug_ - "R UDP: " <> show _clientRequest-}
  {-debug_ - "R UDP: " <> show _decryptedMessage-}

  logSA "R UDP -->:" (initTarget _clientRequest) - \_r -> do
    {-debug_ - "R UDP targetSocket: " <> show _r-}

    let (_targetSocket, _targetSocketAddress) = _r
        (_addr, _) = sockAddr_To_Pair _targetSocketAddress
        _IPLists = getIPLists aEnv

    debug_ - "checking: " <> show _addr
    withChecked_IP_List _addr _IPLists - do
      let _msg = showRelay aSockAddr _clientRequest
      info_ - "RU: " <> _msg

      connect _targetSocket _targetSocketAddress

      send_ _targetSocket _decryptedMessage

      _r <- buildShadowSocksRequest _clientRequest <$> recv_UDP _targetSocket

      {-debug_ - "R UDP <--: " <> show _r-}

      when (_r & isn't _Empty) - do
        _encodeIV <- _cipherBox ^. generate_IV
        _encrypt <- _cipherBox ^. encryptBuilder - _encodeIV

        _encryptedMessage <- processAll _encrypt _r
        sendAllTo aSocket (_encodeIV <> _encryptedMessage) aSockAddr
