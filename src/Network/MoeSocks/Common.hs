{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.Common where

import Control.Lens
import Control.Monad.Writer hiding (listen)
import Data.IP
import Data.Maybe
import Data.Text (Text)
import Data.Text.Lens
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
import Network.Socket hiding (send, recv, recvFrom, sendTo)
import Prelude hiding ((-), take)
import Safe (readMay)
import qualified Data.List as L

showAddressType :: AddressType -> Text
showAddressType (IPv4_address xs) = view (from _Text) - 
                                      concat - L.intersperse "." - 
                                      xs ^.. each . to show
showAddressType (Domain_name x)   = x 
showAddressType (IPv6_address xs) = view (from _Text) -
                                      concat - L.intersperse ":" - 
                                      xs ^.. each . to show

checkForbidden_IP_List :: AddressType -> [IPRange] -> Bool
checkForbidden_IP_List _address@(IPv4_address _) aForbidden_IP_List =
  isJust - 
    do
      _ip <- readMay - showAddressType _address ^. _Text
      findOf (each . _IPv4Range) (isMatchedTo _ip) aForbidden_IP_List

checkForbidden_IP_List _address@(IPv6_address _) aForbidden_IP_List =
  isJust -
    do
      _ip <- readMay - showAddressType _address ^. _Text
      findOf (each . _IPv6Range) (isMatchedTo _ip) - aForbidden_IP_List

checkForbidden_IP_List _ _ = False




showConnectionType :: ConnectionType -> String
showConnectionType TCP_IP_stream_connection = "TCP_Stream"
showConnectionType TCP_IP_port_binding      = "TCP_Bind  "
showConnectionType UDP_port                 = "UDP       "

showRequest :: ClientRequest -> String
showRequest _r =  
                   view _Text (showAddressType (_r ^. addressType))
                <> ":"
                <> show (_r ^. portNumber)

addressType_To_Family :: AddressType -> Maybe Family
addressType_To_Family (IPv4_address _) = Just AF_INET
addressType_To_Family (IPv6_address _) = Just AF_INET6
addressType_To_Family _                = Nothing

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
      _family = _clientRequest ^. addressType . to addressType_To_Family

  
  getSocketWithHint _family _hostName _port _socketType
