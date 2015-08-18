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

local_UDP_ForwardRequestHandler :: MoeConfig -> Forward -> 
                                  ByteString -> (Socket,SockAddr) -> IO ()
local_UDP_ForwardRequestHandler aConfig aForwarding aMessage 
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

      (_encrypt, _decrypt) <- getCipher
                                (aConfig ^. method)
                                (aConfig ^. password)

      {-let (_encrypt, _decrypt) = (pure, pure)-}

      let _bytes = buildShadowSocksRequest _clientRequest aMessage

      {-puts - "L UDP: " <> show _bytes-}

      let _msg = show aSockAddr <> " -> " <> showRequest _clientRequest
      _log - "L U: " <> _msg
      
      send_ _remoteSocket =<< _encrypt (S.Just _bytes)

      (_r, _) <- recv_ _remoteSocket >>= processAll _decrypt 
                                      >>= parseShadowSocksRequest

      {-puts - "L UDP <--: " <> show _r-}
      when (_r & isn't _Empty) - do
        sendAllTo aSocket _r aSockAddr


remote_UDP_RequestHandler:: MoeConfig -> ByteString -> (Socket, SockAddr) 
                                                                      -> IO ()
remote_UDP_RequestHandler aConfig aMessage (aSocket, aSockAddr) = do
  (_encrypt, _decrypt) <- getCipher
                            (aConfig ^. method)
                            (aConfig ^. password)
  

  {-let (_encrypt, _decrypt) = (pure, pure)-}
  
  _msg <- processAll _decrypt aMessage

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
      _encryptedMessage <- processAll _encrypt _r
      sendAllTo aSocket _encryptedMessage aSockAddr
