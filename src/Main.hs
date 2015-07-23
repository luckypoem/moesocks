{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Control.Lens
import Prelude ((.))
import Air.Env hiding ((.), has, take) 

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
import qualified Data.ByteString as B
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Monoid
import Control.Concurrent

pute :: String -> IO ()
pute = hPutStrLn stderr

showBytes :: B.ByteString -> String
showBytes = show . B.unpack

safeSocketHandler :: String -> (Socket -> IO a) -> Socket -> IO a
safeSocketHandler aID f aSocket =
  catch (f aSocket) - \e -> do
      pute - "Exception in " + aID + ": " + show (e :: SomeException)
      sClose aSocket
      throw e


data ClientGreeting = ClientGreeting
  {
    _numberOfAuthenticationMethods :: Word8
  , _authenticationMethods :: [Word8]
  }
  deriving (Show)

makeLenses ''ClientGreeting

data ConnectionType =
    TCP_IP_stream_connection
  | TCP_IP_port_binding
  | UDP_port
  deriving (Show, Eq)

data AddressType = 
    IPv4_address [Word8]
  | Domain_name B.ByteString
  | IPv6_address [Word8]
  deriving (Show, Eq)


data ClientConnectionRequest = ClientConnectionRequest
  {
    _connectionType :: ConnectionType
  , _addressType :: AddressType
  , _portNumber :: (Word8, Word8)
  }
  deriving (Show)

makeLenses ''ClientConnectionRequest

socketHandler:: (Socket, SockAddr) -> IO ()
socketHandler (aSocket, aSockAddr) = do
  puts - "Connected: " + show aSockAddr

  (inputStream, outputStream) <- socketToStreams aSocket

  let socksVersion = 5
      socksHeader = word8 socksVersion
  
  let handshakeParser = do
        socksHeader
        let maxNoOfMethods = 5
        __numberOfAuthenticationMethods <- satisfy (<= maxNoOfMethods)
        __authenticationMethods <- 
          count (fromIntegral __numberOfAuthenticationMethods) anyWord8

        return - 
          ClientGreeting 
            __numberOfAuthenticationMethods
            __authenticationMethods

  let reservedByte = 0
  let connectionParser = do
        socksHeader
        __connectionType <- choice
            [
              TCP_IP_stream_connection <$ word8 1 
            , TCP_IP_port_binding <$ word8 2
            , UDP_port <$ word8 3
            ]

        word8 reservedByte

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
          ClientConnectionRequest
            __connectionType
            __addressType 
            __portNumber

  flip catch (\e -> puts - show (e :: ParseException)) - do
    r <- parseFromStream handshakeParser inputStream
    puts - show r 
  
    let noAuthenticationMethodCode = 0

    if noAuthenticationMethodCode `elem` (r ^. authenticationMethods)
      then do
        let
          write x = do
                      puts - showBytes x
                      S.write (Just - x) outputStream

          push = write . B.singleton
          serverSocks5 = push socksVersion

        serverSocks5
        push noAuthenticationMethodCode

        conn <- parseFromStream connectionParser inputStream
        puts - show conn


        let 
            fromWord8 :: forall t. Binary t => [Word8] -> t
            fromWord8 = decode . runPut . mapM_ put
      
            handleRequest :: Socket -> ClientConnectionRequest -> IO Socket
            handleRequest _localSocket _connection = do
              {-puts "Handle Request"-}

              _remoteSocket <- socket AF_INET Stream defaultProtocol

              let portNumber16 = 
                    fromWord8 - _connection ^. portNumber . _1 :
                                _connection ^. portNumber . _2 : []
                    :: Word16
                  
                  (IPv4_address _address) = _connection ^. addressType
              
              puts - "ports: " <> show portNumber16

              let 
                  _socketAddr = SockAddrInet 
                                  (fromIntegral portNumber16)
                                  (fromWord8 - reverse _address)

              puts - "socketAddr: " <> show _socketAddr

              connect _remoteSocket _socketAddr

              return _remoteSocket
              
        _remoteSocket <- handleRequest aSocket conn
     
        let handleConnection aRemoteSocket = do
              serverSocks5
              push 0
              push reservedByte

              case conn ^. addressType of
                IPv4_address xs ->  do
                                      push 1
                                      write - B.pack xs

                Domain_name x ->    do
                                      push 3
                                      push - fromIntegral (B.length x)
                                      write x

                IPv6_address xs ->  do
                                      push 4
                                      write - B.pack xs

              push - conn ^. portNumber . _1
              push - conn ^. portNumber . _2


              (remoteInputStream, remoteOutputStream) <- 
                socketToStreams aRemoteSocket

              let sendMessageLoop = do
                    sentMessage <- S.read inputStream
                    case sentMessage of
                      Nothing -> return ()
                      Just _ -> do
                        S.write sentMessage remoteOutputStream
                        sendMessageLoop 
                
              let receiveMessageLoop = do
                    receivedMessage <- S.read remoteInputStream 
                    case receivedMessage of
                      Nothing -> return ()
                      Just _ -> do
                        S.write receivedMessage outputStream
                        receiveMessageLoop
             
              sendThreadID <- forkIO sendMessageLoop
              
              receiveMessageLoop
              killThread sendThreadID 

              return ()



        safeSocketHandler "Connection Handler" handleConnection _remoteSocket

        sClose aSocket
        sClose _remoteSocket

      else do
        pute - "Client does not support 0x00: No authentication method"
        sClose aSocket



main :: IO ()
main = do
  puts "Started!"
  mainSocket <- socket AF_INET Stream defaultProtocol
  setSocketOption mainSocket ReuseAddr 1
  bindSocket mainSocket (SockAddrInet 1090 iNADDR_ANY)
  listen mainSocket 1

  let handleConnection _socket = accept _socket >>= fork . socketHandler
      serverLoop = 
        forever . safeSocketHandler "Connection Socket" handleConnection

  safeSocketHandler "Main Socket" serverLoop mainSocket 
