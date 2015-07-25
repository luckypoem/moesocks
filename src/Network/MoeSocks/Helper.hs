{-# LANGUAGE ExistentialQuantification #-}

module Network.MoeSocks.Helper where

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
import Data.Text.Lens

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

catchAll :: IO a -> IO ()
catchAll io = catch (() <$ io) - \e -> 
                pute - "CatcheAll: " <> show (e :: SomeException)

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

pushStream :: (Stream.OutputStream ByteString) -> B.Builder -> IO ()
pushStream s b = do
  _builderStream <- Stream.builderStream s 
  Stream.write (Just b) _builderStream
  Stream.write (Just BE.flush) _builderStream

tryAddr :: Text -> Int -> (SockAddr -> IO a) -> IO ()
tryAddr aHostName aPort = tryAddr' (Just aHostName) (Just aPort) 
                    (Just - defaultHints { addrSocketType = Stream })

tryAddr' :: Maybe Text -> Maybe Int -> Maybe AddrInfo -> 
              (SockAddr -> IO a) -> IO ()
tryAddr' aHostName aPort aHint f = do
  addrInfo <- getAddrInfo 
                    aHint
                    (fmap (view _Text) aHostName) 
                    (fmap show aPort)
  
  puts - "tryAddr':" <> show addrInfo
  let maybeAddr = addrInfo ^? traverse . to addrAddress
  case maybeAddr of 
    Nothing -> pute - "Can not resolve: " <> show aHostName
    Just _addr -> () <$ f _addr


tryParse :: IO a -> IO ()
tryParse io = flip catch (\e -> puts - show (e :: ParseException)) - () <$ io

withSocket :: Socket -> (Socket -> IO ()) -> IO ()
withSocket aSocket f = do
  f aSocket 
  sClose aSocket


sockAddr_To_AddrFamily :: SockAddr -> Family
sockAddr_To_AddrFamily = f where
    f (SockAddrInet  {}) = AF_INET
    f (SockAddrInet6 {}) = AF_INET6
    f (SockAddrUnix  {}) = AF_UNIX

sockAddr_To_Port :: SockAddr -> String
sockAddr_To_Port = f where
    f (SockAddrInet  p _) = show p
    f (SockAddrInet6 p _ _ _) = show p
    f (SockAddrUnix {}) = ""

is_Inet :: AddrInfo -> Bool
is_Inet x =
  let f = addrFamily x
  in
  case f of
    AF_INET -> True
    _ -> False

initSocketForType :: SockAddr -> SocketType -> IO Socket 
initSocketForType aSockAddr aSocketType = 
    socket (sockAddr_To_AddrFamily aSockAddr) aSocketType defaultProtocol


initSocket :: SockAddr -> IO Socket 
initSocket = flip initSocketForType Stream

clamp :: (Integral i) => i -> ByteString -> ByteString
clamp i x = S.take (fromIntegral i) - x <> S.replicate (fromIntegral i) 0
