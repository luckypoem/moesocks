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
import qualified Data.List as L

showAddressType :: AddressType -> Text
showAddressType (IPv4_address xs) = view (from _Text) - 
                                      concat - L.intersperse "." - 
                                      map show - xs ^.. each
showAddressType (Domain_name x)   = x 
showAddressType (IPv6_address xs) = view (from _Text) -
                                      concat - L.intersperse ":" - 
                                      map show - xs ^.. each

maybeIPv4Range :: IPRange -> Maybe (AddrRange IPv4)
maybeIPv4Range (IPv4Range x) = Just x
maybeIPv4Range _ = Nothing

maybeIPv6Range :: IPRange -> Maybe (AddrRange IPv6)
maybeIPv6Range (IPv6Range x) = Just x
maybeIPv6Range _ = Nothing

_IPv4Range :: Prism IPRange IPRange (AddrRange IPv4) (AddrRange IPv4)
_IPv4Range = prism' IPv4Range maybeIPv4Range

_IPv6Range :: Prism IPRange IPRange (AddrRange IPv6) (AddrRange IPv6)
_IPv6Range = prism' IPv6Range maybeIPv6Range

checkForbidden_IP_List :: AddressType -> [IPRange] -> Bool
checkForbidden_IP_List _address@(IPv4_address _) aForbidden_IP_List =
  let _ip = (read - showAddressType _address ^. _Text)
      _ranges = aForbidden_IP_List ^.. traverse . _IPv4Range
  in

  isJust - findOf folded (isMatchedTo _ip) _ranges
checkForbidden_IP_List _address@(IPv6_address _) aForbidden_IP_List =
  let _ip = (read - showAddressType _address ^. _Text)
      _ranges = aForbidden_IP_List ^.. traverse . _IPv6Range
  in

  isJust - findOf folded (isMatchedTo _ip) _ranges
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
