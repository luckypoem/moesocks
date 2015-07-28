module Network.MoeSocks.BuilderAndParser where

import Control.Lens
import Data.Attoparsec.ByteString
import Data.Monoid
import Data.Word
import Network.MoeSocks.Constant
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
import Prelude hiding ((-), take)
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as B


socksVersion :: Word8
socksVersion = 5

socksHeader :: Parser Word8
socksHeader = word8 socksVersion

greetingParser :: Parser ClientGreeting
greetingParser = do
  socksHeader
  let maxNoOfMethods = 5
  _numberOfAuthenticationMethods <- satisfy (<= maxNoOfMethods)

  ClientGreeting <$>
    count (fromIntegral _numberOfAuthenticationMethods) anyWord8

greetingReplyBuilder :: B.Builder
greetingReplyBuilder =  B.word8 socksVersion
                     <> B.word8 _No_authentication

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

connectionParser :: Parser ClientRequest 
connectionParser = do
  socksHeader
  requestParser

connectionReplyBuilder :: ClientRequest -> B.Builder
connectionReplyBuilder _clientRequest = 
      B.word8 socksVersion
  <>  B.word8 _Request_Granted 
  <>  B.word8 _ReservedByte
  <>  addressTypeBuilder (_clientRequest ^. addressType)
  <>  foldMapOf each B.word8 (_clientRequest ^. portNumber)

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
  <>  B.word8 _ReservedByte
  <>  addressTypeBuilder (aClientRequest ^. addressType)
  <>  foldMapOf each B.word8 (aClientRequest ^. portNumber)

shadowsocksRequestBuilder :: ClientRequest -> B.Builder
shadowsocksRequestBuilder aClientRequest =
      addressTypeBuilder (aClientRequest ^. addressType)
  <>  foldMapOf each B.word8 (aClientRequest ^. portNumber)

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

portParser :: Parser (Word8, Word8)
portParser = (,) <$> anyWord8 <*> anyWord8


shadowsocksRequestParser :: Parser ClientRequest
shadowsocksRequestParser = do
  __addressType <- addressTypeParser
  __portNumber <- (,) <$> anyWord8 <*> anyWord8
  pure - 
          ClientRequest
            TCP_IP_stream_connection
            __addressType 
            __portNumber
