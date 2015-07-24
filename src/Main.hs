{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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

syncLock :: MVar ()
syncLock = unsafePerformIO - newEmptyMVar

sync :: IO a -> IO a
sync io = do
  putMVar syncLock ()
  io <* takeMVar syncLock

puts :: String -> IO ()
puts = sync . putStrLn


pute :: String -> IO ()
pute = sync . hPutStrLn stderr

showBytes :: ByteString -> String
showBytes = show . S.unpack

fromWord8 :: forall t. Binary t => [Word8] -> t
fromWord8 = decode . runPut . mapM_ put
      
safeSocketHandler :: String -> (Socket -> IO a) -> Socket -> IO a
safeSocketHandler aID f aSocket =
  catch (f aSocket) - \e -> do
      pute - "Exception in " + aID + ": " + show (e :: SomeException)
      sClose aSocket
      throw e

waitBoth :: IO a -> IO b -> IO ()
waitBoth x y = do
  (xThreadID, xLock) <- do
    _lock <- newEmptyMVar
    _threadID <- 
      forkFinally x - const - putMVar _lock ()

    return (_threadID, _lock)

  yThreadID <- 
    forkFinally y - const - killThread xThreadID 
                
  takeMVar xLock 
  killThread yThreadID

pushStream :: (S.OutputStream ByteString) -> B.Builder -> IO ()
pushStream s b = do
  _builderStream <- S.builderStream s 
  S.write (Just b) _builderStream
  S.write (Just BE.flush) _builderStream

data ClientGreeting = ClientGreeting
  {
    _authenticationMethods :: [Word8]
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
  | Domain_name ByteString
  | IPv6_address [Word8]
  deriving (Show, Eq)


data ClientRequest = ClientRequest
  {
    _connectionType :: ConnectionType
  , _addressType :: AddressType
  , _portNumber :: (Word8, Word8)
  }
  deriving (Show)

makeLenses ''ClientRequest

localRequestHandler:: (Socket, SockAddr) -> IO ()
localRequestHandler (aSocket, aSockAddr) = do
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
             
        _remoteSocket <- connectRemote [127, 0, 0, 1] 1190
     
        let handleLocal _remoteSocket = do
              let
                write x = S.write (Just - x) outputStream
                push = write . S.singleton

              push socksVersion
              push 0
              push reservedByte

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
              
              return ()



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
        
        return ()
  
  safeSocketHandler "Target Connection Handler" 
    handleTarget _targetSocket
 
  sClose _targetSocket
  sClose aSocket

main :: IO ()
main = do
  puts "Started!"
  localSocket <- socket AF_INET Stream defaultProtocol
  setSocketOption localSocket ReuseAddr 1
  bindSocket localSocket (SockAddrInet 1090 iNADDR_ANY)
  listen localSocket 1

  let handleLocal _socket = accept _socket >>= fork . localRequestHandler
      socksServerLoop = 
        forever . safeSocketHandler "Local Connection Socket" handleLocal


  remoteSocket <- socket AF_INET Stream defaultProtocol
  setSocketOption remoteSocket ReuseAddr 1
  bindSocket remoteSocket (SockAddrInet 1190 iNADDR_ANY)
  listen remoteSocket 1

  let handleRemote _socket = accept _socket >>= fork . remoteRequestHandler
      remoteServerLoop = 
        forever . safeSocketHandler "Remote Connection Socket" handleRemote
  
  safeSocketHandler "Local Socket" (\_localSocket ->
    safeSocketHandler "Remote Socket" (\_remoteSocket ->
      waitBoth 
        (socksServerLoop _localSocket) 
        (remoteServerLoop _remoteSocket)
        ) remoteSocket) localSocket

