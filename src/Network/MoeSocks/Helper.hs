{-# LANGUAGE ExistentialQuantification #-}

module Network.MoeSocks.Helper where

import Control.Lens
import Prelude ((.))
import qualified Prelude as P
import Air.Env hiding ((.), has, take, puts) 

import Network.Socket
import Control.Monad
import Control.Applicative
import System.Posix.Signals
import Control.Exception
import System.IO
import System.IO.Streams.Network
import qualified System.IO.Streams as Stream
import System.IO.Streams (InputStream, OutputStream)
import System.IO.Streams.List
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
import qualified Data.ByteString.Lazy as LB

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lens
import Control.Monad.IO.Class

_Debug :: Bool
_Debug = True

syncLock :: MVar ()
syncLock = unsafePerformIO - newEmptyMVar

sync :: IO a -> IO a
sync io = do
  putMVar syncLock ()
  io <* takeMVar syncLock

puts :: String -> IO ()
puts 
  | _Debug = sync . putStrLn
  | otherwise = const - pure ()

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

catchIO:: IO a -> IO ()
catchIO io = catch (() <$ io) - \e ->
                pute - "Catch IO: " <> show (e :: IOException)
                

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
  
  {-puts - "tryAddr':" <> show addrInfo-}
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

sockAddr_To_Host :: SockAddr -> String
sockAddr_To_Host = f where
    f s@(SockAddrInet  _ _) = P.takeWhile (/= ':') - show s
    f s@(SockAddrInet6 _ _ _ _) = reverse -
                                  P.dropWhile (== ':') -
                                  P.dropWhile (/= ':') - 
                                  reverse - show s
    f (SockAddrUnix s) = s

initSocketForType :: SockAddr -> SocketType -> IO Socket 
initSocketForType aSockAddr aSocketType = 
    socket (sockAddr_To_AddrFamily aSockAddr) aSocketType defaultProtocol


initSocket :: SockAddr -> IO Socket 
initSocket = flip initSocketForType Stream

clamp :: (Integral i) => i -> ByteString -> ByteString
clamp i x = S.take (fromIntegral i) - x <> S.replicate (fromIntegral i) 0

-- first bit is length
splitTokens :: Int -> ByteString -> [ByteString]
splitTokens l x  
  | l <= 1 = [x]
  | x == mempty = []
  | otherwise = 
      let 
          byteLength = l + (-1)
          (y, z) = S.splitAt byteLength x
          length_y_byte = fromIntegral - S.length y
      in
      clamp l (S.cons length_y_byte y) : splitTokens l z

tokenizeStream :: (Integral n) => n -> (ByteString -> ByteString) ->
                    InputStream ByteString -> IO (InputStream ByteString)
tokenizeStream n f input = 
  Stream.map (splitTokens - fromIntegral n) input 
      >>= concatLists >>= Stream.map f

decodeToken :: ByteString -> ByteString
decodeToken x
  | x == mempty = mempty
  | otherwise = S.take (fromIntegral - S.head x) - S.tail x

chunkStream :: (Integral n) => n -> InputStream ByteString -> 
                        IO (InputStream ByteString)
chunkStream n input = Stream.fromGenerator - go
  where
    l = fromIntegral n 
    go = liftIO (Stream.read input) >>= maybe (return $! ()) chunk
    chunk x 
      | S.length x >= l = Stream.yield (S.take l x) >> chunk (S.drop l x)
      | otherwise = 
          liftIO (Stream.read input) >>= 
            maybe (Stream.yield x) (chunk . (x <>))

detokenizeStream :: (Integral n) => n -> (ByteString -> ByteString) -> 
                                      InputStream ByteString -> 
                                      IO (InputStream ByteString)
detokenizeStream n f input = Stream.map (decodeToken . f) =<< 
                                    chunkStream n input

builder_To_ByteString :: B.Builder -> ByteString
builder_To_ByteString = LB.toStrict . B.toLazyByteString

