{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.UDP where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Writer hiding (listen)
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Network.MoeSocks.BuilderAndParser
import Network.MoeSocks.Common
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
import Network.MoeSocks.Type.Config
import Network.Socket hiding (send, recv, recvFrom, sendTo)
import Network.Socket.ByteString
import Prelude hiding ((-), take)
import qualified Data.ByteString as S
import qualified Data.Strict as S


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
                                -> Forward
                                -> ByteString
                                -> (Socket,SockAddr)
                                -> IO ()
local_UDP_ForwardRequestHandler aEnv
                                aForwarding
                                aMessage
                                (aSocket, aSockAddr) = do

  let _c = aEnv ^. config
      _cipherBox = aEnv ^. cipherBox

  let _clientRequest = ClientRequest
                          UDP_Port
                          (DomainName - aForwarding ^.
                            forwardTargetHost)
                          (aForwarding ^. forwardTargetPort)

  {-debug_ - "L UDP: " <> show _clientRequest-}

  let _addr = _clientRequest ^. addressType
      _forbidden_IPs = aEnv ^. options . forbidden_IPs

  debug_ - "checking: " <> show _addr <> " ? " <> show _forbidden_IPs

  withCheckedForbidden_IP_List _addr _forbidden_IPs - do
    _sa <- getSocket (_c ^. remoteHost) (_c ^. remotePort) Datagram

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

        _response <- recv_ _remoteSocket

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
      _options = aEnv ^. options

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
        _forbidden_IPs = _options ^. forbidden_IPs

    debug_ - "checking: " <> show _addr <> " ? " <> show _forbidden_IPs
    withCheckedForbidden_IP_List _addr _forbidden_IPs - do
      let _msg = showRelay aSockAddr _clientRequest
      info_ - "RU: " <> _msg

      connect _targetSocket _targetSocketAddress

      send_ _targetSocket _decryptedMessage

      _r <- buildShadowSocksRequest _clientRequest <$> recv_ _targetSocket

      {-debug_ - "R UDP <--: " <> show _r-}

      when (_r & isn't _Empty) - do
        _encodeIV <- _cipherBox ^. generate_IV
        _encrypt <- _cipherBox ^. encryptBuilder - _encodeIV

        _encryptedMessage <- processAll _encrypt _r
        sendAllTo aSocket (_encodeIV <> _encryptedMessage) aSockAddr
