{-# LANGUAGE ExistentialQuantification #-}

module Network.MoeSocks.Helper where

import Control.Concurrent
import Control.Exception
import Control.Lens
import Data.Binary
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text (Text)
import Data.Text.Lens
import Network.MoeSocks.Internal.ShadowSocks.Encrypt
import Network.Socket
import Prelude hiding (take, (-)) 
import System.IO
import System.IO.Unsafe
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Strict.Lens as TS
import qualified System.IO.Streams as Stream

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
      
logSocketWithAddress :: String -> IO (Socket, SockAddr) -> 
                        ((Socket, SockAddr) -> IO a) -> IO a
logSocketWithAddress aID _init f =
  catch (bracket _init (close .fst) f) - \e -> do
      pute - "Exception in " <> aID <> ": " <> show (e :: SomeException)
      throw e

logSA:: String -> IO (Socket, SockAddr) -> 
                        ((Socket, SockAddr) -> IO a) -> IO a
logSA = logSocketWithAddress

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
  let _init = do
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
    _init
    handleError
    action
                

pushStream :: (Stream.OutputStream ByteString) -> B.Builder -> IO ()
pushStream s b = do
  _builderStream <- Stream.builderStream s 
  Stream.write (Just b) _builderStream
  Stream.write (Just BE.flush) _builderStream



getSocket :: (Integral i, Show i) => HostName -> i -> SocketType ->
                                      IO (Socket, SockAddr)
getSocket aHost aPort aSocketType = do
    maybeAddrInfo <- firstOf folded <$>
                  getAddrInfo (Just hints) (Just aHost) (Just $ show aPort)

    case maybeAddrInfo of
      Nothing -> error - "Error in getSocket for: " <> aHost <> ":" <> 
                              show aPort
      Just addrInfo -> do
          let family     = addrFamily addrInfo
          let socketType = addrSocketType addrInfo
          let protocol   = addrProtocol addrInfo
          let address    = addrAddress addrInfo

          _socket <- socket family socketType protocol

          pure (_socket, address)
          
  where
    hints = defaultHints {
              addrFlags = [AI_ADDRCONFIG, AI_NUMERICSERV]
            , addrSocketType = aSocketType
            }

builder_To_ByteString :: B.Builder -> ByteString
builder_To_ByteString = LB.toStrict . B.toLazyByteString

type Cipher = ByteString -> IO ByteString 

getCipher :: Text -> Text -> IO (Cipher, Cipher)
getCipher method password =
  getEncDec (method ^. _Text) (review TS.utf8 password)


portNumber16 :: (Word8, Word8) -> Word16
portNumber16 pair = fromWord8 - toListOf both pair

duplicateKey :: (Eq a) => (a, a) -> [(a, b)] -> [(a, b)]
duplicateKey (_from, _to) l = 
  case lookup _from l of
    Nothing -> l
    Just v -> (_to,v) : l
