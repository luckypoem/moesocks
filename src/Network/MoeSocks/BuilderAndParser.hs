module Network.MoeSocks.BuilderAndParser where

import Control.Lens
import Prelude hiding ((-), take)

import Data.Attoparsec.ByteString
import Data.Word
import qualified Data.ByteString as S
import Data.Monoid
import qualified Data.ByteString.Builder as B

import Network.MoeSocks.Type
import Network.MoeSocks.Constant
import Network.MoeSocks.Helper


addressTypeBuilder :: AddressType -> B.Builder
addressTypeBuilder aAddressType = 
  case aAddressType of
    IPv4_address _address -> 
                          B.word8 1
                       <> foldMap B.word8 _address 
    Domain_name x ->   
                          B.word8 3
                       <> B.word8 (fromIntegral (S.length x))
                       <> B.byteString x

    IPv6_address xs ->  
                          B.word8 4
                       <> B.byteString (S.pack xs)



connectionType_To_Word8 :: ConnectionType -> Word8
connectionType_To_Word8 TCP_IP_stream_connection = 1
connectionType_To_Word8 TCP_IP_port_binding = 2
connectionType_To_Word8 UDP_port = 3

requestBuilder :: ClientRequest -> B.Builder
requestBuilder aClientRequest = 
     B.word8 (connectionType_To_Word8 - aClientRequest ^. connectionType)
  <> B.word8 _ReservedByte
  <> addressTypeBuilder (aClientRequest ^. addressType)
  <> foldMapOf each B.word8 (aClientRequest ^. portNumber)

addressTypeParser :: Parser AddressType
addressTypeParser = choice
  [
    IPv4_address <$>  do
                        word8 1
                        count 4 anyWord8
  
  , Domain_name <$>   do 
                        word8 3
                        let maxDomainNameLength = 32
                        _nameLength <- satisfy (<= maxDomainNameLength)
                        take - fromIntegral _nameLength

  , IPv6_address <$>  do
                        word8 4 
                        count 16 anyWord8
  ]

requestParser :: Parser ClientRequest
requestParser = do
  __connectionType <- choice
      [
        TCP_IP_stream_connection <$ word8 1 
      , TCP_IP_port_binding <$ word8 2
      , UDP_port <$ word8 3
      ]

  word8 _ReservedByte
  __addressType <- addressTypeParser
  __portNumber <- (,) <$> anyWord8 <*> anyWord8
  pure - 
          ClientRequest
            __connectionType
            __addressType 
            __portNumber
