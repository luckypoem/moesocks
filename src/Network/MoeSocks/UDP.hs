{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.UDP where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Writer hiding (listen)
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Network.MoeSocks.BuilderAndParser
import Network.MoeSocks.Common
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
import Network.Socket hiding (send, recv, recvFrom, sendTo)
import Network.Socket.ByteString
import Prelude hiding ((-), take)
import qualified Data.Strict as S


buildShadowSocksRequest :: ClientRequest -> ByteString -> ByteString
buildShadowSocksRequest aClientRequest aMessage =
  let _header = shadowSocksRequestBuilder aClientRequest
  in  
  builder_To_ByteString _header <> aMessage

parseShadowSocksRequest :: ByteString -> IO (ByteString, ClientRequest)
parseShadowSocksRequest aMessage =
  case parse (shadowSocksRequestParser UDP_port) aMessage of
    Done _i _r -> pure (_i, _r)
    _ -> throwIO - ParseException -
            "R Failed to parse UDP request"


processAll :: Cipher -> ByteString -> IO ByteString
processAll f x = 
  (<>) <$> f (S.Just x) <*> f S.Nothing

local_UDP_ForwardRequestHandler :: CipherBox 
                                -> MoeConfig 
                                -> Forward 
                                -> ByteString 
                                -> (Socket,SockAddr) 
                                -> IO ()
local_UDP_ForwardRequestHandler aCipherBox aConfig aForwarding aMessage 
                                                    (aSocket, aSockAddr) = do

  let _c = aConfig

  let _clientRequest = ClientRequest
                          UDP_port
                          (Domain_name - aForwarding ^. 
                            forwardRemoteHost)
                          (aForwarding ^. forwardRemotePort)
  
  {-puts - "L UDP: " <> show _clientRequest-}

  _sa <- getSocket (_c ^. remote) (_c ^. remotePort) Datagram

  logSA "L UDP -->:" (pure _sa) - 
    \(_remoteSocket, _remoteAddr) -> do
      connect _remoteSocket _remoteAddr

      _encodeIV <- aCipherBox ^. generateIV 
      _encrypt <- aCipherBox ^. encryptBuilder - _encodeIV

      {-let (_encrypt, _decrypt) = (pure, pure)-}

      let _bytes = buildShadowSocksRequest _clientRequest aMessage

      {-puts - "L UDP: " <> show _bytes-}

      let _msg = show aSockAddr <> " -> " <> showRequest _clientRequest
      _log - "L U: " <> _msg
      
      _eMsg <- _encrypt (S.Just _bytes)

      send_ _remoteSocket - _encodeIV <> _eMsg

      _response <- recv_ _remoteSocket 

      let (_decodeIV, _responseMsg) = S.splitAt (aCipherBox ^. ivLength) 
                                        _response
      _decrypt <- aCipherBox ^. decryptBuilder - _decodeIV
      
      (_r, _) <- processAll _decrypt _responseMsg
                                      >>= parseShadowSocksRequest

      when (_r & isn't _Empty) - do
        sendAllTo aSocket _r aSockAddr


remote_UDP_RequestHandler :: CipherBox 
                          -> MoeConfig 
                          -> ByteString 
                          -> (Socket, SockAddr) 
                          -> IO ()
remote_UDP_RequestHandler aCipherBox _ aMessage (aSocket, aSockAddr) = do
  let (_decodeIV, _eMsg) = S.splitAt (aCipherBox ^. ivLength) 
                                       aMessage 

  _decrypt <- aCipherBox ^. decryptBuilder - _decodeIV
  _msg <- processAll _decrypt _eMsg

  (_decryptedMessage, _clientRequest) <- parseShadowSocksRequest _msg
  
  {-puts - "R UDP: " <> show _clientRequest-}
  {-puts - "R UDP: " <> show _decryptedMessage-}
  
  logSA "R UDP -->:" (initTarget _clientRequest) - \_r -> do
    let (_clientSocket, _clientAddr) = _r

    {-puts - "R UDP clientSocket: " <> show _r-}
    
    let _msg = show aSockAddr <> " -> " <> showRequest _clientRequest
    _log - "R U: " <> _msg

    connect _clientSocket _clientAddr
    
    send_ _clientSocket _decryptedMessage

    _r <- buildShadowSocksRequest _clientRequest <$> recv_ _clientSocket

    {-puts - "R UDP <--: " <> show _r-}

    when (_r & isn't _Empty) - do
      _encodeIV <- aCipherBox ^. generateIV 
      _encrypt <- aCipherBox ^. encryptBuilder - _encodeIV
      
      _encryptedMessage <- processAll _encrypt _r
      sendAllTo aSocket (_encodeIV <> _encryptedMessage) aSockAddr
