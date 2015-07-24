{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

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
import qualified System.IO.Streams as S
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
import Data.Text.Lens

import Network.MoeSocks.Config
import Network.MoeSocks.Helper
import Network.MoeSocks.Type


localRequestHandler:: MoeConfig -> (Socket, SockAddr) -> IO ()
localRequestHandler config (aSocket, aSockAddr) = do
  puts - "Connected: " + show aSockAddr

  (inputStream, outputStream) <- socketToStreams aSocket

  let socksVersion = 5
      socksHeader = word8 socksVersion
  
  let greetingParser = do
        socksHeader
        let maxNoOfMethods = 5
        __numberOfAuthenticationMethods <- satisfy (<= maxNoOfMethods)
        __authenticationMethods <- 
          count (fromIntegral __numberOfAuthenticationMethods) anyWord8

        return - 
          ClientGreeting 
            __authenticationMethods

  let _ReservedByte = 0
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
  
    let noAuthenticationMethodCode = 0

    if noAuthenticationMethodCode `elem` (r ^. authenticationMethods)
      then do
        pushStream outputStream - B.word8 socksVersion
                                <> B.word8 noAuthenticationMethodCode


        conn <- parseFromStream connectionParser inputStream
        puts - show conn


        let 
            connectRemote :: [Word8] -> Int -> IO Socket
            connectRemote _address _port = do
              {-puts "Local Handle Request"-}

              _remoteSocket <- socket AF_INET Stream defaultProtocol
              let 
                  _socketAddr = SockAddrInet 
                                  (fromIntegral _port)
                                  (fromWord8 - reverse - 
                                      map fromIntegral _address)
              
              connect _remoteSocket _socketAddr

              return _remoteSocket
             
        _remoteSocket <- connectRemote [127, 0, 0, 1] - config ^. serverPort
     
        let handleLocal _remoteSocket = do
              let
                write x = S.write (Just - x) outputStream
                push = write . S.singleton

              push socksVersion
              let _Request_Granted = 0
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

                  _header = foldMap B.word8 _address 
                            <> foldMapOf both B.word8 (conn ^. portNumber)

              pushStream remoteOutputStream _header

              waitBoth
                (S.connect inputStream remoteOutputStream)
                (S.connect remoteInputStream outputStream)
              
              pure ()



        safeSocketHandler "Local Request Handler" 
          handleLocal _remoteSocket

        sClose aSocket
        sClose _remoteSocket

      else do
        pute - "Client does not support 0x00: No authentication method"
        sClose aSocket


remoteRequestHandler:: (Socket, SockAddr) -> IO ()
remoteRequestHandler (aSocket, aSockAddr) = do
  puts - "Remote Connected: " + show aSockAddr
  (remoteInputStream, remoteOutputStream) <- socketToStreams aSocket
  let headerParser = do
        _address <- count 4 anyWord8
        _port <- (,) <$> anyWord8 <*> anyWord8
        
        return (_address, _port)

        
  (_address, _port) <- parseFromStream headerParser remoteInputStream

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
          (S.connect remoteInputStream targetOutputStream)
          (S.connect targetInputStream remoteOutputStream)
        
        pure ()
  
  safeSocketHandler "Target Connection Handler" 
    handleTarget _targetSocket
 
  sClose _targetSocket
  sClose aSocket

main :: IO ()
main = do
  puts "Started!"
  config <- pure defaultMoeConfig

  let 
      _localName = config ^. local . unpacked
      _localPort = show - config ^. localPort
  
  localAddrInfo <- getAddrInfo Nothing 
                    (Just _localName) 
                    (Just _localPort)
  
  let maybeLocalAddr = localAddrInfo ^? traverse . to addrAddress

  case maybeLocalAddr of 
    Nothing -> pute - "Can not resolve localhost: " <> _localName

    Just _localAddr -> do

      puts - "localAddr: " <> show _localAddr

      localSocket <- socket AF_INET Stream defaultProtocol
      setSocketOption localSocket ReuseAddr 1
      bindSocket localSocket _localAddr

      listen localSocket 1

      let handleLocal _socket = 
            accept _socket >>= fork . localRequestHandler config

          socksServerLoop = 
            forever . safeSocketHandler "Local Connection Socket" handleLocal


      let 
          _remoteName = config ^. server . unpacked
          _remotePort = show - config ^. serverPort
      
      remoteAddrInfo <- getAddrInfo Nothing 
                        (Just _remoteName) 
                        (Just _remotePort)
      
      let maybeRemoteAddr = remoteAddrInfo ^? traverse . to addrAddress

      case maybeRemoteAddr of
        Nothing -> pute - "Can not resolve remote: " <> _remoteName
        Just _remoteAddr -> do
          puts - "remoteAddr: " <> show _remoteAddr
          remoteSocket <- socket AF_INET Stream defaultProtocol
          setSocketOption remoteSocket ReuseAddr 1
          bindSocket remoteSocket _remoteAddr
          listen remoteSocket 1

          let handleRemote _socket = 
                accept _socket >>= fork . remoteRequestHandler
              remoteServerLoop = 
                forever . 
                safeSocketHandler "Remote Connection Socket" handleRemote
          
          safeSocketHandler "Local Socket" (\_localSocket ->
            safeSocketHandler "Remote Socket" (\_remoteSocket ->
              waitBoth 
                (socksServerLoop _localSocket) 
                (remoteServerLoop _remoteSocket)
                ) remoteSocket) localSocket

