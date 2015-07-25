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
import qualified Data.ByteString.Lazy as LB
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
import Network.MoeSocks.BuilderAndParser

import Data.ByteString.Lens
import System.Random
import qualified Prelude as P
import "cipher-aes" Crypto.Cipher.AES
import Data.Maybe


localRequestHandler:: MoeConfig -> (Socket, SockAddr) -> IO ()
localRequestHandler config (_s, aSockAddr) = withSocket _s - \aSocket -> do
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
        requestParser

  tryParse - do
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
        
        withSocket _remoteSocket - \_remoteSocket -> do
          tryAddr (config ^. server) (config ^. serverPort) - \_remoteAddr -> do
            connect _remoteSocket _remoteAddr

            let handleLocal _remoteSocket = do
                  let
                    write x = Stream.write (Just - x) outputStream
                    push = write . S.singleton

                  push socksVersion
                  push _Request_Granted 
                  push _ReservedByte

                  write - LB.toStrict - B.toLazyByteString -
                      addressTypeBuilder (conn ^. addressType)

                  traverseOf both push - conn ^. portNumber

                  (remoteInputStream, remoteOutputStream) <- 
                    socketToStreams _remoteSocket

                  _stdGen <- newStdGen

                  let _iv = S.pack - P.take _BlockSize - randoms _stdGen
                  puts - "local  IV: " <> show _iv
                  
                  pushStream remoteOutputStream - B.byteString _iv
                  
                  let
                      _aesKey = aesKey config
                      _encrypt = encryptCTR _aesKey _iv
                      _decrypt = decryptCTR _aesKey _iv
                  
                  encryptedRemoteOutputStream <- 
                    Stream.contramap _encrypt remoteOutputStream

                  decryptedRemoteInputStream <-
                    Stream.map _decrypt remoteInputStream

                  let 
                      _header = requestBuilder conn

                  pushStream encryptedRemoteOutputStream _header

                  waitBoth
                    (Stream.connect inputStream encryptedRemoteOutputStream)
                    (Stream.connect decryptedRemoteInputStream outputStream)
                  

            safeSocketHandler "Local Request Handler" 
              handleLocal _remoteSocket


remoteRequestHandler:: MoeConfig -> (Socket, SockAddr) -> IO ()
remoteRequestHandler aConfig (_s, aSockAddr) = withSocket _s - \aSocket -> do
  puts - "Remote Connected: " + show aSockAddr
  (remoteInputStream, remoteOutputStream) <- socketToStreams aSocket

  tryParse - do
    _iv <- parseFromStream (take _BlockSize) remoteInputStream

    puts - "remote IV: " <> show _iv

    let 
        _aesKey = aesKey aConfig
        _encrypt = encryptCTR _aesKey _iv
        _decrypt = decryptCTR _aesKey _iv
    
    encryptedRemoteOutputStream <- 
      Stream.contramap _encrypt remoteOutputStream

    decryptedRemoteInputStream <-
      Stream.map _decrypt remoteInputStream
    
    _clientRequest <- parseFromStream requestParser
                            decryptedRemoteInputStream
    
    let
        connectTarget :: ClientRequest -> IO (Maybe Socket)
        connectTarget _clientRequest = do
          _targetSocket <- socket AF_INET Stream defaultProtocol

          let portNumber16 = fromWord8 - toListOf both 
                              (_clientRequest ^. portNumber) :: Word16
          
          let addressType_To_SockAddr :: ClientRequest -> Either String SockAddr
              addressType_To_SockAddr aClientRequest =
                case aClientRequest ^. addressType of
                  IPv4_address _address -> Right - SockAddrInet 
                                            (fromIntegral portNumber16)
                                            (fromWord8 - reverse _address)

                  Domain_name x -> case x ^? TS.utf8 . _Text of
                                      Nothing -> Left -  
                                                    "Invalid Domain Name: "
                                                    <> show x
                                      Just _name -> Right - SockAddrUnix _name
                  IPv6_address xs -> 
                                      let rs = reverse xs
                                      in
                                      Right - SockAddrInet6 
                                        (fromIntegral portNumber16)
                                        0
                                        ( fromWord8 - P.take 4 - rs
                                        , fromWord8 - P.drop 4 - P.take 4 - rs
                                        , fromWord8 - P.drop 8 - P.take 4 - rs
                                        , fromWord8 - P.drop 12 - rs
                                        )
                                        0
          
          case addressType_To_SockAddr _clientRequest of
            Left err -> do
              pute err
              pure - Nothing
            Right _socketAddr -> do
              puts - "Connecting Target: " <> show _socketAddr
              connect _targetSocket _socketAddr
              pure - Just _targetSocket

    _targetSocket <- connectTarget _clientRequest
    
    forM_ _targetSocket - flip withSocket - \_targetSocket -> do
      let 
          handleTarget _targetSocket = do
            (targetInputStream, targetOutputStream) <- 
              socketToStreams _targetSocket

            waitBoth
              (Stream.connect decryptedRemoteInputStream targetOutputStream)
              (Stream.connect targetInputStream encryptedRemoteOutputStream)
            
      safeSocketHandler "Target Connection Handler" 
        handleTarget _targetSocket


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
      
      catchAll - do
        safeSocketHandler "Local Socket" (\_localSocket ->
          safeSocketHandler "Remote Socket" (\_remoteSocket ->
            waitBoth 
              (socksServerLoop _localSocket) 
              (remoteServerLoop _remoteSocket)
              ) remoteSocket) localSocket

