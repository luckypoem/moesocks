{-# LANGUAGE OverloadedStrings #-}

module Network.MoeSocks.BuilderAndParser where

import Control.Lens
import Data.Attoparsec.ByteString
import Data.Binary
import Data.Binary.Put
import Data.Maybe
import Data.Monoid
import Data.Text.Lens
import Data.Text.Strict.Lens (utf8)
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
import Network.Socket
import Prelude hiding ((-), take)
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as B
import qualified Prelude as P


_No_authentication :: Word8
_No_authentication = 0

_Request_Granted :: Word8
_Request_Granted = 0

_ReservedByte :: Word8
_ReservedByte = 0

_SpaceCode :: Word8
_SpaceCode = 32


socksVersion :: Word8
socksVersion = 5

-- Builder

greetingReplyBuilder :: B.Builder
greetingReplyBuilder =  B.word8 socksVersion
                     <> B.word8 _No_authentication



sockAddr_To_Pair :: SockAddr -> (AddressType, Port)
sockAddr_To_Pair aSockAddr = case aSockAddr of
  SockAddrInet _port _host ->
                                    let
                                        _r@(_a, _b, _c, _d) =
                                          decode . runPut . put - _host
                                          :: (Word8, Word8, Word8, Word8)
                                    in

                                    ( IPv4_Address - flip4 _r
                                    , fromIntegral _port
                                    )

  SockAddrInet6 _port _ _host _ ->
                                    let
                                        _r@(_a, _b, _c, _d, _e, _f, _g, _h) =
                                          decode . runPut . put - _host
                                          :: (Word16, Word16, Word16, Word16
                                              , Word16, Word16, Word16, Word16)
                                    in

                                    ( IPv6_Address - _r ^.. each
                                    , fromIntegral _port
                                    )

  SockAddrUnix x ->
                                    let
                                        _host = P.takeWhile (/= ':') x
                                        _port = x & reverse
                                                  & P.takeWhile (/= ':')
                                                  & reverse
                                    in

                                    ( DomainName - (_host & review _Text)
                                    , fromMaybe 0 - _port ^? _Show
                                    )

  x ->
                                    error - "SockAddrCan not implemented: "
                                            <> show x


portBuilder :: (Integral i) => i -> B.Builder
portBuilder i =
  let _i = fromIntegral i :: Word16
  in
  foldMapOf both B.word8 -
    (decode - runPut - put _i :: (Word8, Word8))

connectionReplyBuilder :: SockAddr -> B.Builder
connectionReplyBuilder aSockAddr =
  let _r@(__addressType, _port) = sockAddr_To_Pair aSockAddr
  in
      B.word8 socksVersion
  <>  B.word8 _Request_Granted
  <>  B.word8 _ReservedByte
  <>  addressTypeBuilder __addressType
  <>  portBuilder _port

addressTypeBuilder :: AddressType -> B.Builder
addressTypeBuilder aAddressType =
  case aAddressType of
    IPv4_Address _address ->
                          B.word8 1
                       <> foldMapOf each B.word8 _address
    DomainName x ->
                          B.word8 3
                       <> B.word8 (fromIntegral (S.length (review utf8 x)))
                       <> B.byteString (review utf8 x)

    IPv6_Address _address ->
                          B.word8 4
                       <> foldMapOf each B.word16BE _address



connectionType_To_Word8 :: ConnectionType -> Word8
connectionType_To_Word8 TCP_IP_StreamConnection = 1
{-connectionType_To_Word8 TCP_IP_PortBinding = 2-}
connectionType_To_Word8 UDP_Port = 3



requestBuilder :: ClientRequest -> B.Builder
requestBuilder aClientRequest =
      B.word8 (connectionType_To_Word8 - aClientRequest ^. connectionType)
  <>  B.word8 _ReservedByte
  <>  addressTypeBuilder (aClientRequest ^. addressType)
  <>  portBuilder (aClientRequest ^. portNumber)

shadowSocksRequestBuilder :: ClientRequest -> B.Builder
shadowSocksRequestBuilder aClientRequest =
      addressTypeBuilder (aClientRequest ^. addressType)
  <>  portBuilder (aClientRequest ^. portNumber)






-- Parser

socksHeader :: Parser Word8
socksHeader = word8 socksVersion

greetingParser :: Parser ClientGreeting
greetingParser = do
  socksHeader
  let maxNoOfMethods = 5
  _numberOfAuthenticationMethods <- satisfy (<= maxNoOfMethods)

  ClientGreeting <$>
    count (fromIntegral _numberOfAuthenticationMethods) anyWord8


portParser :: Parser Int
portParser = do
  __portNumberPair <- (,) <$> anyWord8 <*> anyWord8
  pure - portPairToInt __portNumberPair


requestParser :: Parser ClientRequest
requestParser = do
  __connectionType <- choice
      [
        TCP_IP_StreamConnection <$ word8 1
      {-, TCP_IP_PortBinding <$ word8 2-}
      , UDP_Port <$ word8 3
      ]

  word8 _ReservedByte
  __addressType <- addressTypeParser
  __portNumber <- portParser
  pure -
          ClientRequest
            __connectionType
            __addressType
            __portNumber

connectionParser :: Parser ClientRequest
connectionParser = do
  socksHeader
  requestParser


anyWord16 :: Parser Word16
anyWord16 = do
  _b <- (,) <$> anyWord8 <*> anyWord8
  pure - decode - runPut - put _b


addressTypeParser :: Parser AddressType
addressTypeParser = choice
  [
    IPv4_Address <$>  do
                        word8 1
                        _a <- anyWord8
                        _b <- anyWord8
                        _c <- anyWord8
                        _d <- anyWord8
                        pure - (_a, _b, _c, _d)

  , DomainName <$>   do
                        word8 3
                        _nameLength <- anyWord8
                        view utf8 <$> (take - fromIntegral _nameLength)

  , IPv6_Address <$>  do
                        word8 4
                        _r <- count 8 anyWord16
                        {-pure - trace ("parsed IPv6: " <> show _r) _r-}
                        pure _r
  ]


shadowSocksRequestParser :: ConnectionType -> Parser ClientRequest
shadowSocksRequestParser _connectionType = do
  _addressType <- addressTypeParser
  _portNumber <- portParser

  pure -
          ClientRequest
            _connectionType
            _addressType
            _portNumber
