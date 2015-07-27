{-# LANGUAGE ExistentialQuantification #-}

module Network.MoeSocks.Helper where

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad.IO.Class
import Data.Binary
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text (Text)
import Data.Text.Lens
import qualified Data.Text.Strict.Lens as TS
import Network.Socket
import Prelude hiding (take, (-)) 
import System.IO
import System.IO.Streams (InputStream)
import System.IO.Streams.Attoparsec
import System.IO.Streams.List
import System.IO.Unsafe
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.ByteString.Lazy as LB
import qualified Prelude as P
import qualified System.IO.Streams as Stream
import Network.MoeSocks.Internal.ShadowSocks.Encrypt
import qualified Data.ByteString.Char8 as C

-- BEGIN backports

infixr 0 -
(-) :: (a -> b) -> a -> b
(-) = ($)

-- END backports

_Debug :: Bool
_Debug = True

boolToMaybe :: Bool -> Maybe ()
boolToMaybe True = Just ()
boolToMaybe False = Nothing

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
      
logSocket :: String -> IO Socket -> (Socket -> IO a) -> IO a
logSocket aID _init f =
  catch (bracket _init close f) - \e -> do
      pute - "Exception in " <> aID <> ": " <> show (e :: SomeException)
      throw e

catchAllLog :: String -> IO a -> IO ()
catchAllLog aID io = catch (() <$ io) - \e -> 
                pute - "CatcheAll in " <> aID <> ": " 
                    <> show (e :: SomeException)

catchAll :: IO a -> IO ()
catchAll io = catch (() <$ io) - \e -> 
                pute - "CatcheAll: " <> show (e :: SomeException)

catchIO:: IO a -> IO ()
catchIO io = catch (() <$ io) - \e ->
                pute - "Catch IO: " <> show (e :: IOException)
                

waitBoth :: IO a -> IO b -> IO ()
waitBoth x y = do
  let init = do
        (xThreadID, xLock) <- do
          _lock <- newEmptyMVar
          _threadID <- 
            forkFinally x -
               const - putMVar _lock ()

          return (_threadID, _lock)

        yThreadID <- 
          forkFinally y - const - killThread xThreadID 

        return (xThreadID, xLock, yThreadID)

  let handleError (xThreadID, _, yThreadID) = do
      killThread yThreadID
      killThread xThreadID

  let action (_, xLock, yThreadID) = do
        takeMVar xLock 
        killThread yThreadID

  bracket 
    init
    handleError
    action
                

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



sockAddr_To_AddrFamily :: SockAddr -> Family
sockAddr_To_AddrFamily = f where
    f (SockAddrInet  {}) = AF_INET
    f (SockAddrInet6 {}) = AF_INET6
    f (SockAddrUnix  {}) = AF_UNIX
    f _ = AF_INET

sockAddr_To_Port :: SockAddr -> String
sockAddr_To_Port = f where
    f (SockAddrInet  p _) = show p
    f (SockAddrInet6 p _ _ _) = show p
    f (SockAddrUnix {}) = ""
    f _ = ""

sockAddr_To_Host :: SockAddr -> String
sockAddr_To_Host = f where
    f s@(SockAddrInet  _ _) = P.takeWhile (/= ':') - show s
    f s@(SockAddrInet6 _ _ _ _) = reverse -
                                  P.dropWhile (== ':') -
                                  P.dropWhile (/= ':') - 
                                  reverse - show s
    f (SockAddrUnix s) = s
    f _ = ""

initSocketForType :: SockAddr -> SocketType -> IO Socket 
initSocketForType aSockAddr aSocketType = 
    socket (sockAddr_To_AddrFamily aSockAddr) aSocketType defaultProtocol


initSocket :: SockAddr -> IO Socket 
initSocket = flip initSocketForType Stream


builder_To_ByteString :: B.Builder -> ByteString
builder_To_ByteString = LB.toStrict . B.toLazyByteString

type Cipher = ByteString -> IO ByteString 

getCipher :: Text -> Text -> IO (Cipher, Cipher)
getCipher method password =
  getEncDec (method ^. _Text) (review TS.utf8 password)

getIVLength :: Text -> Int
getIVLength = iv_len . view _Text

{-type Decode = (Int, ByteString -> IO (ByteString -> IO ByteString))-}
{-type Encode = (IO (), ByteString -> IO ByteString)-}

{-getDecode :: Text -> Text -> Text -> Decode-}
{-getDecode method password iv = -}
  {-let _ivLength = getIVLength method-}
      {-_initDecode = getSSLDec (method ^. _Text) (review TS.utf8 password)-}
  {-in-}
  {-(_ivLength, _initDecode)-}

{-getEncode :: Text -> Text -> Text -> Encode-}
{-getEncode method password iv = -}
  {-let _ivLength = getIVLength method-}
      {-_encode = getSSLEnc (method ^. _Text) (review TS.utf8 password)-}
      {-_initEncode = _encode iv-}
  {-in-}
  
  {-(_ivLength, _initEncode)-}

