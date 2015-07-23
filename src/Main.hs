module Main where

import Control.Lens
import Prelude ((.))
import Air.Env hiding ((.)) 

import Network.Socket
import Control.Monad
import System.Posix.Signals
import Control.Exception
import System.IO

pute :: String -> IO ()
pute = hPutStrLn stderr

safeSocket :: String -> Socket -> (Socket -> IO a) -> IO a
safeSocket aID aSocket f =
  catch (f aSocket) - \e -> do
      pute - "Exception in " + aID + ": " + show (e :: SomeException)
      sClose aSocket
      throw e

main :: IO ()
main = do
  puts "Started!"
  _mainSocket <- socket AF_INET Stream defaultProtocol
  setSocketOption _mainSocket ReuseAddr 1
  bindSocket _mainSocket (SockAddrInet 1091 iNADDR_ANY)
  listen _mainSocket 1


  let handler aSocket = safeSocket "Connection Socket" aSocket - \_socket ->
                  accept _socket >>= fork . socketHandler

  safeSocket "Main Socket" _mainSocket - forever . handler 
    

socketHandler:: (Socket, SockAddr) -> IO ()
socketHandler (_socket, _sockAddr) = do
  puts - "Connected: " + show _sockAddr
  send _socket - "Hi: " + show _sockAddr + "!\n"
  sClose _socket

