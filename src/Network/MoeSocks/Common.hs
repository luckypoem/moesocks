{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.MoeSocks.Common where

import Control.Lens
import Control.Monad.Writer hiding (listen)
import Data.IP
import Data.Maybe
import Data.Text.Lens
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
import Network.Socket hiding (send, recv, recvFrom, sendTo)
import Prelude hiding ((-), take)


makePrisms ''IPRange

check_IP_List :: AddressType -> [IPRange] -> Bool
check_IP_List _address@(IPv4_Address _) a_IP_List =
  isJust -
    do
      _ip <- show _address  ^? _Show
      findOf (each . _IPv4Range) (isMatchedTo _ip) a_IP_List

check_IP_List _address@(IPv6_Address _) a_IP_List =
  isJust -
    do
      _ip <- show _address ^? _Show
      findOf (each . _IPv6Range) (isMatchedTo _ip) a_IP_List

check_IP_List _ _ = False

withChecked_IP_List :: AddressType -> ([IPRange], Maybe [IPRange])
                        -> IO a -> IO ()
withChecked_IP_List aAddressType (aDenyList, aAllowList) aIO =
  if check_IP_List aAddressType aDenyList
    then error_ - show aAddressType
                <> " is in the denied list"
    else
      case aAllowList of
        Nothing -> () <$ aIO
        Just _allowList ->
          if not - check_IP_List aAddressType _allowList
            then error_ - show aAddressType
                    <> " is NOT in the allowed list"
            else
              () <$ aIO

getIPLists :: Env -> ([IPRange], Maybe [IPRange])
getIPLists aEnv = (aEnv ^. denyList <> aEnv ^. forbidden_IPs, mempty)

showConnectionType :: ConnectionType -> String
showConnectionType TCP_IP_StreamConnection = "TCP_Stream"
-- showConnectionType TCP_IP_PortBinding      = "TCP_Bind  "
showConnectionType UDP_Port                = "UDP       "

showRequest :: ClientRequest -> String
showRequest _r =
                   _r ^. addressType . to show
                <> ":"
                <> _r ^. portNumber . to show

showRelay :: SockAddr -> ClientRequest -> String
showRelay aSockAddr aClientRequest =
      show aSockAddr <> " -> " <> showRequest aClientRequest

addressType_To_Family :: AddressType -> Family
addressType_To_Family (IPv4_Address _) = AF_INET
addressType_To_Family (IPv6_Address _) = AF_INET6
addressType_To_Family _                = AF_UNSPEC

_SocketType :: Prism' SocketType ConnectionType
_SocketType = prism' connectionType_To_SocketType socketType_To_connectionType
  where
    connectionType_To_SocketType :: ConnectionType -> SocketType
    connectionType_To_SocketType TCP_IP_StreamConnection = Stream
    {-connectionType_To_SocketType TCP_IP_PortBinding      = NoSocketType-}
    connectionType_To_SocketType UDP_Port                = Datagram

    socketType_To_connectionType :: SocketType -> Maybe ConnectionType
    socketType_To_connectionType Stream = Just TCP_IP_StreamConnection
    socketType_To_connectionType Datagram = Just UDP_Port
    socketType_To_connectionType _ = Nothing


initTarget :: ClientRequest -> IO (Socket, SockAddr)
initTarget _clientRequest = do
  let
      _socketType = _clientRequest ^. connectionType . re _SocketType
      _hostName   = _clientRequest ^. addressType . to show . re _Text
      _port       = _clientRequest ^. portNumber
      _family     = _clientRequest ^. addressType . to addressType_To_Family

  getSocketWithHint _family _hostName _port _socketType


setSocketConfig:: Env -> Socket -> IO ()
setSocketConfig aEnv aSocket = do
  setSocketOption aSocket NoDelay 1
  when (aEnv ^. socketOption_TCP_NOTSENT_LOWAT) - do
    tryIO "setSocket_TCP_NOTSENT_LOWAT" - setSocket_TCP_NOTSENT_LOWAT aSocket
    pure ()
