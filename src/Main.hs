module Main where

import Control.Lens
import Prelude ((.))
import Air.Env hiding ((.)) 

import Network.Socket
import Control.Monad
import System.Posix.Signals
import Control.Exception
import System.IO


main :: IO ()
main = do
  puts "Started!"
  _socket <- socket AF_INET Stream defaultProtocol

  setSocketOption _socket ReuseAddr 1
  
  bindSocket _socket (SockAddrInet 1091 iNADDR_ANY)

  listen _socket 1

  let handler = 
        catch (accept _socket >>= fork . socketHandler)- \e -> do
            hPutStrLn stderr - "Handler Exception: " + show (e :: SomeException)
            sClose _socket
            throw e
          

  catch (forever (handler >> sleep 1)) - \e -> do
    hPutStrLn stderr - "Exception: " + show (e :: SomeException)
    sClose _socket
    throw e
    

socketHandler:: (Socket, SockAddr) -> IO ()
socketHandler (_socket, _) = do
  send _socket "Accepting: \n"
  sClose _socket

