{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.Common where

import Control.Lens
import Control.Monad.Writer hiding (listen)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Lens
import Data.Text.Strict.Lens (utf8)
import Network.MoeSocks.Helper
import qualified Network.MoeSocks.Encrypt as E
import Network.MoeSocks.Type
import Network.Socket hiding (send, recv, recvFrom, sendTo)
import Prelude hiding ((-), take)
import qualified Data.List as L
import qualified Data.Strict as S

plainCipher :: Cipher
plainCipher = E.plainCipher

showAddressType :: AddressType -> Text
showAddressType (IPv4_address xs) = view (from _Text) - 
                                      concat - L.intersperse "." - 
                                      map show - xs ^.. each
showAddressType (Domain_name x)   = x 
showAddressType (IPv6_address xs) = view (from _Text) -
                                      concat - L.intersperse ":" - 
                                      map show - xs ^.. each

showConnectionType :: ConnectionType -> String
showConnectionType TCP_IP_stream_connection = "TCP_Stream"
showConnectionType TCP_IP_port_binding      = "TCP_Bind  "
showConnectionType UDP_port                 = "UDP       "

showRequest :: ClientRequest -> String
showRequest _r =  
                   view _Text (showAddressType (_r ^. addressType))
                <> ":"
                <> show (_r ^. portNumber)

initTarget :: ClientRequest -> IO (Socket, SockAddr)
initTarget _clientRequest = do
  let 
      connectionType_To_SocketType :: ConnectionType -> SocketType
      connectionType_To_SocketType TCP_IP_stream_connection = Stream
      connectionType_To_SocketType TCP_IP_port_binding = NoSocketType
      connectionType_To_SocketType UDP_port = Datagram
         
      _socketType = connectionType_To_SocketType -
                      _clientRequest ^. connectionType


      _hostName = _clientRequest ^. addressType . to showAddressType
      _port = _clientRequest ^. portNumber

  
  getSocket _hostName _port _socketType
