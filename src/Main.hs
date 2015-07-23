{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Prelude ((.))
import Air.Env hiding ((.)) 

import Network.Socket
import Control.Monad
import System.Posix.Signals
import Control.Exception
import System.IO
import System.IO.Streams.Network
import qualified System.IO.Streams as S

pute :: String -> IO ()
pute = hPutStrLn stderr

safeSocketHandler :: String -> (Socket -> IO a) -> Socket -> IO a
safeSocketHandler aID f aSocket =
  catch (f aSocket) - \e -> do
      pute - "Exception in " + aID + ": " + show (e :: SomeException)
      sClose aSocket
      throw e

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
    

socketHandler:: (Socket, SockAddr) -> IO ()
socketHandler (aSocket, aSockAddr) = do
  puts - "Connected: " + show aSockAddr

  (_, outputStream) <- socketToStreams aSocket
  S.write (Just "ByteString Stream!\n") outputStream
  sClose aSocket

