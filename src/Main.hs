{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Prelude ((.))
import Air.Env hiding ((.)) 

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

pute :: String -> IO ()
pute = hPutStrLn stderr

safeSocketHandler :: String -> (Socket -> IO a) -> Socket -> IO a
safeSocketHandler aID f aSocket =
  catch (f aSocket) - \e -> do
      pute - "Exception in " + aID + ": " + show (e :: SomeException)
      sClose aSocket
      throw e

    
data ClientGreeting = ClientGreeting
  {
    _socksVersionNumber :: Word8
  , _numberOfAuthenticationMethods :: Word8
  , _authenticationMethods :: [Word8]
  }
  deriving (Show)

makeLenses ''ClientGreeting

socketHandler:: (Socket, SockAddr) -> IO ()
socketHandler (aSocket, aSockAddr) = do
  puts - "Connected: " + show aSockAddr

  (inputStream, outputStream) <- socketToStreams aSocket

       
  let parser = do
        __socksVersionNumber <- word8 5
        let maxNoOfMethods = 10
        __numberOfAuthenticationMethods <- satisfy - (<= maxNoOfMethods)
        __authenticationMethods <- 
          count (fromIntegral __numberOfAuthenticationMethods) anyWord8

        return - 
          ClientGreeting 
            __socksVersionNumber 
            __numberOfAuthenticationMethods
            __authenticationMethods

  flip catch (\e -> puts - show (e :: ParseException)) - do
    r <- parseFromStream parser inputStream
    puts - show r 
  
  S.write (Just "ByteString Stream!\n") outputStream
  sClose aSocket



main :: IO ()
main = do
  puts "Started!"
  mainSocket <- socket AF_INET Stream defaultProtocol
  setSocketOption mainSocket ReuseAddr 1
  bindSocket mainSocket (SockAddrInet 1090 iNADDR_ANY)
  listen mainSocket 1

  let handler _socket = accept _socket >>= fork . socketHandler
      serverLoop = forever . safeSocketHandler "Connection Socket" handler

  safeSocketHandler "Main Socket" serverLoop mainSocket 
