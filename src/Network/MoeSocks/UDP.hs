{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.UDP where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Writer hiding (listen)
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Text.Lens
import Network.MoeSocks.BuilderAndParser
import Network.MoeSocks.Common
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
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
  case parse (shadowSocksRequestParser UDP_port) aMessage of
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
local_UDP_ForwardRequestHandler aEnv aForwarding aMessage 
                                                    (aSocket, aSockAddr) = do

  let _c = aEnv ^. config
      _cipherBox = _cipherBox

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

      _encodeIV <- _cipherBox ^. generateIV 
      _encrypt <- _cipherBox ^. encryptBuilder - _encodeIV

      {-let (_encrypt, _decrypt) = (pure, pure)-}

      let _bytes = buildShadowSocksRequest _clientRequest aMessage

      {-puts - "L UDP: " <> show _bytes-}

      let _msg = show aSockAddr <> " -> " <> showRequest _clientRequest
      _log - "L U: " <> _msg
      
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
remote_UDP_RequestHandler aEnv aMessage (aSocket, aSockAddr) = do
  let _cipherBox = aEnv ^. cipherBox
      _options = aEnv ^. options

  let (_decodeIV, _eMsg) = S.splitAt (_cipherBox ^. ivLength) 
                                       aMessage 

  _decrypt <- _cipherBox ^. decryptBuilder - _decodeIV
  _msg <- processAll _decrypt _eMsg

  (_decryptedMessage, _clientRequest) <- parseShadowSocksRequest _msg
  
  {-puts - "R UDP: " <> show _clientRequest-}
  {-puts - "R UDP: " <> show _decryptedMessage-}
  
  let _addr = _clientRequest ^. addressType
  if checkForbidden_IP_List _addr - _options ^. forbidden_IP
    then pute - showAddressType _addr ^. _Text 
                <> " is in forbidden-ip list"
    else
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
        _encodeIV <- _cipherBox ^. generateIV 
        _encrypt <- _cipherBox ^. encryptBuilder - _encodeIV
        
        _encryptedMessage <- processAll _encrypt _r
        sendAllTo aSocket (_encodeIV <> _encryptedMessage) aSockAddr
