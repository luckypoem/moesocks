{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Network.MoeSocks.UDP where

import Control.Lens
import Control.Monad
import Control.Monad.Writer hiding (listen)
import Data.ByteString (ByteString)
import Network.MoeSocks.Helper
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
  
  puts - show _clientRequest
  _sa <- getSocket (_c ^. remote) (_c ^. remotePort) Datagram

  logSA "L UDP -->:" (pure _sa) - 
    \(_remoteSocket, _remoteAddr) -> do
      connect _remoteSocket _remoteAddr
      send_ _remoteSocket aMessage

      _r <- recv_ _remoteSocket

      puts - "L UDP <--: " <> show _r
      when (_r & isn't _Empty) - do
        sendAllTo aSocket _r aSockAddr

remoteUDPRequestHandler:: MoeConfig -> ByteString -> (Socket, SockAddr) 
                                                                      -> IO ()
remoteUDPRequestHandler _ aMessage (aSocket, aSockAddr) = do
  _sa <- getSocket "localhost" (53 :: Int) Datagram
  logSA "R UDP -->:" (pure _sa) -
    \(_clientSocket, _clientAddr) -> do
      connect _clientSocket _clientAddr
      send_ _clientSocket aMessage

      _r <- recv_ _clientSocket

      puts - "R UDP <--: " <> show _r

      when (_r & isn't _Empty) - do
        sendAllTo aSocket _r aSockAddr
