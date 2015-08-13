{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Network.MoeSocks.UDP where

import Control.Lens
import Control.Monad
import Data.Attoparsec.ByteString
import Control.Monad.Writer hiding (listen)
import Control.Exception
import Data.ByteString (ByteString)
import Network.MoeSocks.Helper
import Network.MoeSocks.BuilderAndParser
import Network.MoeSocks.Common
import Network.MoeSocks.Type
import Network.Socket hiding (send, recv, recvFrom, sendTo)
import Network.Socket.ByteString
import Prelude hiding ((-), take)




forwardUDPRequestHandler :: MoeConfig -> Forward -> 
                                  ByteString -> (Socket,SockAddr) -> IO ()
forwardUDPRequestHandler aConfig aForwarding aMessage 
                                                    (aSocket, aSockAddr) = do

  let _c = aConfig

  let _clientRequest = ClientRequest
                          UDP_port
                          (Domain_name - aForwarding ^. 
                            forwardRemoteHost)
                          (aForwarding ^. forwardRemotePort)
  
  puts - "L UDP: " <> show _clientRequest

  _sa <- getSocket (_c ^. remote) (_c ^. remotePort) Datagram

  logSA "L UDP -->:" (pure _sa) - 
    \(_remoteSocket, _remoteAddr) -> do
      connect _remoteSocket _remoteAddr

      (_encrypt, _decrypt) <- getCipher
                                (aConfig ^. method)
                                (aConfig ^. password)

      {-let (_encrypt, _decrypt) = (pure, pure)-}

      let _msg = buildShadowSocksRequest _clientRequest aMessage

      puts - "L UDP: " <> show _msg
      
      send_ _remoteSocket =<< _encrypt _msg

      (_r, _) <- recv_ _remoteSocket >>= _decrypt >>= parseShadowSocksRequest

      puts - "L UDP <--: " <> show _r
      when (_r & isn't _Empty) - do
        sendAllTo aSocket _r aSockAddr

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

remoteUDPRequestHandler:: MoeConfig -> ByteString -> (Socket, SockAddr) 
                                                                      -> IO ()
remoteUDPRequestHandler aConfig aMessage (aSocket, aSockAddr) = do
  (_encrypt, _decrypt) <- getCipher
                            (aConfig ^. method)
                            (aConfig ^. password)
  

  {-let (_encrypt, _decrypt) = (pure, pure)-}
  
  _msg <- _decrypt aMessage

  (_decryptedMessage, _clientRequest) <- parseShadowSocksRequest _msg
  
  puts - "R UDP: " <> show _clientRequest
  puts - "R UDP: " <> show _decryptedMessage
  
  logSA "R UDP -->:" (initTarget _clientRequest) - \_r -> do
    let (_clientSocket, _clientAddr) = _r

    puts - "R UDP clientSocket: " <> show _r

    connect _clientSocket _clientAddr
    
    send_ _clientSocket _decryptedMessage

    _r <- buildShadowSocksRequest _clientRequest <$> recv_ _clientSocket

    puts - "R UDP <--: " <> show _r

    when (_r & isn't _Empty) - do
      _encryptedMessage <- _encrypt _r
      sendAllTo aSocket _encryptedMessage aSockAddr
