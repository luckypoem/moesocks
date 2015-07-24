{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Control.Lens
import Prelude ((.))
import Air.Env hiding ((.), has, take, puts) 

import Network.Socket
import Control.Monad
import Control.Applicative
import System.Posix.Signals
import Control.Exception
import System.IO
import System.IO.Streams.Network
import qualified System.IO.Streams as Stream
import Data.Attoparsec.ByteString
import System.IO.Streams.Attoparsec
import Data.Word
import qualified Data.ByteString as S
import Data.ByteString (ByteString)
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Monoid
import Control.Concurrent
import System.IO.Unsafe
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as BE

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Strict.Lens as TS
import Data.Text.Lens

import Network.MoeSocks.Config
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
import Network.MoeSocks.Constant

import Data.ByteString.Lens
import System.Random
import qualified Prelude as P
import "cipher-aes" Crypto.Cipher.AES

_BlockSize :: Int
_BlockSize = 16

_KeySize :: Int
_KeySize = 32

type Cipher = (ByteString -> ByteString, ByteString -> ByteString)

aesKey :: MoeConfig -> AES
aesKey _config =
  let __password = 
        _config ^. password & review TS.utf8 :: ByteString

      _key = S.take _KeySize - __password <> S.pack (replicate _KeySize 0)
  in
  initAES _key

localRequestHandler:: MoeConfig -> (Socket, SockAddr) -> IO ()
localRequestHandler config (aSocket, aSockAddr) = do
  puts - "Connected: " + show aSockAddr

  (inputStream, outputStream) <- socketToStreams aSocket

  let socksVersion = 5
      socksHeader = word8 socksVersion
  
  let greetingParser = do
        socksHeader
        let maxNoOfMethods = 5
        _numberOfAuthenticationMethods <- satisfy (<= maxNoOfMethods)

        ClientGreeting <$>
          count (fromIntegral _numberOfAuthenticationMethods) anyWord8

  let connectionParser = do
        socksHeader
        __connectionType <- choice
            [
              TCP_IP_stream_connection <$ word8 1 
            , TCP_IP_port_binding <$ word8 2
            , UDP_port <$ word8 3
            ]

        word8 _ReservedByte

        __addressType <- choice
            [
              IPv4_address <$>  do
                                  word8 1
                                  count 4 anyWord8
            
            , Domain_name <$>   do 
                                  word8 3
                                  let maxDomainNameLength = 32
                                  _nameLength <- satisfy 
                                                    (<= maxDomainNameLength)
                                  take - fromIntegral _nameLength

            , IPv6_address <$>  do
                                  word8 4 
                                  count 16 anyWord8
            ]

        __portNumber <- (,) <$> anyWord8 <*> anyWord8

        return - 
          ClientRequest
            __connectionType
            __addressType 
            __portNumber

  flip catch (\e -> puts - show (e :: ParseException)) - do
    r <- parseFromStream greetingParser inputStream
    puts - show r 
    if not - _No_authentication `elem` (r ^. authenticationMethods)
      then do
        pute - "Client does not support 0x00: No authentication method"
        sClose aSocket

      else do
        pushStream outputStream - B.word8 socksVersion
                                <> B.word8 _No_authentication


        conn <- parseFromStream connectionParser inputStream
        puts - show conn

        _remoteSocket <- socket AF_INET Stream defaultProtocol
        
        tryAddr (config ^. server) (config ^. serverPort) - \_remoteAddr -> do
          connect _remoteSocket _remoteAddr

          let handleLocal _remoteSocket = do
                let
                  write x = Stream.write (Just - x) outputStream
                  push = write . S.singleton

                push socksVersion
                push _Request_Granted 
                push _ReservedByte

                case conn ^. addressType of
                  IPv4_address xs ->  do
                                        push 1
                                        write - S.pack xs

                  Domain_name x ->    do
                                        push 3
                                        push - fromIntegral (S.length x)
                                        write x

                  IPv6_address xs ->  do
                                        push 4
                                        write - S.pack xs

                traverseOf both push - conn ^. portNumber

                (remoteInputStream, remoteOutputStream) <- 
                  socketToStreams _remoteSocket

                let IPv4_address _address = conn ^. addressType

                    _password = review TS.utf8 - config ^. password
                    _fill = S.replicate (_BlockSize + (-S.length _password)) 0 
                    
                _stdGen <- newStdGen

                let _iv = S.pack - P.take _BlockSize - randoms _stdGen


                    _aesKey = aesKey config
                    _encrypt = encryptCTR _aesKey _iv
                    _decrypt = decryptCTR _aesKey _iv

                    _header =    B.byteString _iv
                              <> foldMap B.word8 _address 
                              <> foldMapOf each B.word8 (conn ^. portNumber)

                pushStream remoteOutputStream _header

                waitBoth
                  (Stream.connect inputStream remoteOutputStream)
                  (Stream.connect remoteInputStream outputStream)
                

          safeSocketHandler "Local Request Handler" 
            handleLocal _remoteSocket

        sClose aSocket
        sClose _remoteSocket



remoteRequestHandler:: MoeConfig -> (Socket, SockAddr) -> IO ()
remoteRequestHandler aConfig (aSocket, aSockAddr) = do
  puts - "Remote Connected: " + show aSockAddr
  (remoteInputStream, remoteOutputStream) <- socketToStreams aSocket

  _iv <- parseFromStream (take _BlockSize) remoteInputStream

  let headerParser = do
        _address <- count 4 anyWord8
        _port <- (,) <$> anyWord8 <*> anyWord8
        
        return (_address, _port)

  (_address, _port) <- parseFromStream headerParser remoteInputStream

  -- puts - showBytes _token
  
  let 
      _aesKey = aesKey aConfig
      _encrypt = encryptCTR _aesKey _iv
      _decrypt = decryptCTR _aesKey _iv
  
  let
      connectTarget :: [Word8] -> (Word8, Word8) -> IO Socket
      connectTarget _address _port = do
        _targetSocket <- socket AF_INET Stream defaultProtocol

        let portNumber16 = fromWord8 - toListOf both _port :: Word16
            _socketAddr = SockAddrInet 
                            (fromIntegral portNumber16)
                            (fromWord8 - reverse _address)


        puts - "Connecting Target: " <> show _socketAddr
        connect _targetSocket _socketAddr

        return _targetSocket

  _targetSocket <- connectTarget _address _port

  let 
      handleTarget _targetSocket = do
        (targetInputStream, targetOutputStream) <- 
          socketToStreams _targetSocket

        waitBoth
          (Stream.connect remoteInputStream targetOutputStream)
          (Stream.connect targetInputStream remoteOutputStream)
        
        pure ()
  
  safeSocketHandler "Target Connection Handler" 
    handleTarget _targetSocket
 
  sClose _targetSocket
  sClose aSocket


main :: IO ()
main = do
  puts "Started!"
  config <- pure defaultMoeConfig

  tryAddr (config ^. local) (config ^. localPort) - \_localAddr -> do
    puts - "localAddr: " <> show _localAddr

    localSocket <- socket AF_INET Stream defaultProtocol
    setSocketOption localSocket ReuseAddr 1
    bindSocket localSocket _localAddr

    listen localSocket 1

    let handleLocal _socket = 
          accept _socket >>= 
            fork . localRequestHandler config

        socksServerLoop = 
          forever . safeSocketHandler "Local Connection Socket" handleLocal

    
    tryAddr (config ^. server) (config ^. serverPort) - \_remoteAddr -> do
      puts - "remoteAddr: " <> show _remoteAddr

      remoteSocket <- socket AF_INET Stream defaultProtocol
      setSocketOption remoteSocket ReuseAddr 1
      bindSocket remoteSocket _remoteAddr
      listen remoteSocket 1

      let handleRemote _socket = 
            accept _socket >>= 
              fork . remoteRequestHandler config

          remoteServerLoop = 
            forever . 
            safeSocketHandler "Remote Connection Socket" handleRemote
      
      safeSocketHandler "Local Socket" (\_localSocket ->
        safeSocketHandler "Remote Socket" (\_remoteSocket ->
          waitBoth 
            (socksServerLoop _localSocket) 
            (remoteServerLoop _remoteSocket)
            ) remoteSocket) localSocket

