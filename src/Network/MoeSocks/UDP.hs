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
forwardUDPRequestHandler aConfig aTCPForwarding aMessage 
                                                    (aSocket, aSockAddr) = do

  let _c = aConfig

  let _clientRequest = ClientRequest
                          UDP_port
                          (Domain_name - aTCPForwarding ^. 
                            forwardRemoteHost)
                          (aTCPForwarding ^. forwardRemotePort)
  
  puts - "L UDP: " <> show _clientRequest

  _sa <- getSocket (_c ^. remote) (_c ^. remotePort) Datagram

  logSA "L UDP -->:" (pure _sa) - 
    \(_remoteSocket, _remoteAddr) -> do
      connect _remoteSocket _remoteAddr

      (_encrypt, _decrypt) <- getCipher
                                (aConfig ^. method)
                                (aConfig ^. password)

      {-let (_encrypt, _decrypt) = (pure, pure)-}

      let 
          _header = shadowSocksRequestBuilder _clientRequest
      
      
      let _msg = builder_To_ByteString _header <> aMessage

      puts - "L UDP: " <> show _msg
      
      send_ _remoteSocket =<< _encrypt _msg

      _r <- recv_ _remoteSocket >>= _decrypt

      puts - "L UDP <--: " <> show _r
      when (_r & isn't _Empty) - do
        sendAllTo aSocket _r aSockAddr

remoteUDPRequestHandler:: MoeConfig -> ByteString -> (Socket, SockAddr) 
                                                                      -> IO ()
remoteUDPRequestHandler aConfig aMessage (aSocket, aSockAddr) = do
  (_encrypt, _decrypt) <- getCipher
                            (aConfig ^. method)
                            (aConfig ^. password)
  

  {-let (_encrypt, _decrypt) = (pure, pure)-}
  
  _msg <- _decrypt aMessage

  (_decryptedMessage, _clientRequest) <-
    case parse (shadowSocksRequestParser UDP_port) _msg of
      Done _i _r -> pure (_i, _r)
      _ -> throwIO - ParseException -
            "R Failed to parse UDP request"
  
  puts - "R UDP: " <> show _clientRequest
  puts - "R UDP: " <> show _decryptedMessage
  
  logSA "R UDP -->:" (initTarget _clientRequest) - \_r -> do
    let (_clientSocket, _clientAddr) = _r

    puts - "R UDP clientSocket: " <> show _r

    connect _clientSocket _clientAddr
    
    send_ _clientSocket _decryptedMessage

    _r <- recv_ _clientSocket

    puts - "R UDP <--: " <> show _r

    when (_r & isn't _Empty) - do
      _encryptedMessage <- _encrypt _r
      sendAllTo aSocket _encryptedMessage aSockAddr
