{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.MoeSocks.Helper where

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Binary
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text (Text)
import Data.Text.Lens
import Data.Text.Strict.Lens (utf8)
import Network.MoeSocks.Internal.ShadowSocks.Encrypt
import Network.Socket
import Prelude hiding (take, (-)) 
import System.IO.Streams (InputStream, OutputStream)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.ByteString.Lazy as LB
import qualified System.IO.Streams as Stream

import System.Log.Logger

-- BEGIN backports

infixr 0 -
(-) :: (a -> b) -> a -> b
(-) = ($)

-- END backports

type IB = InputStream ByteString
type OB = OutputStream ByteString

_Debug :: Bool
_Debug = False

flip4 :: (a, b, c, d) -> (d, c, b, a)
flip4 (_a, _b, _c, _d) = (_d, _c, _b, _a)

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
puts = sync . debugM "moe" . ("😽  " <>)

pute :: String -> IO ()
pute = sync . errorM "moe" . ("😾  " <>)

_log :: String -> IO ()
_log = sync . infoM "moe" . ("😺  " <>)

_say :: String -> IO ()
_say = sync . noticeM "moe" . ("😼  " <>)

puteT :: Text -> IO ()
puteT = pute . view _Text

showBytes :: ByteString -> String
showBytes = show . S.unpack

      
logClose :: String -> Socket -> IO ()
logClose aID aSocket = do
      puts - "Closing socket " <> aID
      close aSocket 

logSocketWithAddress :: String -> IO (Socket, SockAddr) -> 
                        ((Socket, SockAddr) -> IO a) -> IO a
logSocketWithAddress aID _init f = do
  catch (bracket _init (logClose aID . fst) f) - 
      \(e :: SomeException) -> do
      pute - "logSocket: Exception in " <> aID <> ": " <> show e
      throw e

logSA:: String -> IO (Socket, SockAddr) -> 
                        ((Socket, SockAddr) -> IO a) -> IO a
logSA = logSocketWithAddress

logSocket :: String -> IO Socket -> (Socket -> IO a) -> IO a
logSocket aID _init f =
  catch (bracket _init (logClose aID) f) - \e -> do
      pute - "Exception in " <> aID <> ": " <> show (e :: SomeException)
      throw e

catchExceptAsyncLog :: String -> IO a -> IO ()
catchExceptAsyncLog aID io = catches (() <$ io) 
                [ 
                  Handler - \(e :: AsyncException) -> do
                            pute - "ASyncException in " 
                                    <> aID
                                    <> " : " <> show e
                            throw e
                , Handler - \(e :: SomeException) -> 
                            pute - "CatcheAll in "
                                    <> aID
                                    <> " : " <> show e
                ]

catchIO:: String -> IO a -> IO ()
catchIO aID io = catch (() <$ io) - \e ->
                pute - "Catch IO in " <> aID <> ": " 
                  <> show (e :: IOException)
                

wrapIO :: (Maybe String, IO c) -> IO c
wrapIO (s,  _io) = do
  pure s
  forM_ s - puts . ("+ " <>)
  _io 
    <* (forM_ s - puts . ("- " <>))
                
runBoth :: IO () -> IO () -> IO ()
runBoth x y = do
  runBothDebug (Nothing, x) (Nothing, y)

runBothDebug :: (Maybe String, IO ()) -> (Maybe String, IO ()) -> IO ()
runBothDebug x y = do
  let _x = wrapIO x
      _y = wrapIO y

  _threadXDone <- newEmptyMVar
  _threadYDone <- newEmptyMVar

  let _init = do
        xThreadID <-
          forkFinally _x -
             const - putMVar _threadXDone ()

        yThreadID <- 
          forkFinally _y - const - do
            _threadXRunning <- isEmptyMVar _threadXDone
            putMVar _threadYDone ()
            when _threadXRunning - killThread xThreadID 

        return (xThreadID, yThreadID)

  let handleError (xThreadID, yThreadID) = do
        killThread yThreadID
        killThread xThreadID

  let action (_, yThreadID) = do
        takeMVar _threadXDone 
        _threadYRunning <- isEmptyMVar _threadYDone
        when _threadYRunning - killThread yThreadID

  bracket 
    _init
    handleError
    action

pushStream :: (OutputStream ByteString) -> B.Builder -> IO ()
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

          puts - "Getting socket: " <> show address

          pure (_socket, address)
          
  where
    hints = defaultHints {
              addrFlags = [AI_ADDRCONFIG, AI_NUMERICSERV]
            , addrSocketType = aSocketType
            , addrFamily = AF_INET
            }

builder_To_ByteString :: B.Builder -> ByteString
builder_To_ByteString = LB.toStrict . B.toLazyByteString

type Cipher = ByteString -> IO ByteString 

getCipher :: Text -> Text -> IO (Cipher, Cipher)
getCipher method password =
  getEncDec method (review utf8 password)


fromWord8 :: forall t. Binary t => [Word8] -> t
fromWord8 = decode . runPut . mapM_ put

portPairToInt :: (Word8, Word8) -> Int
portPairToInt = fromIntegral . portPairToWord16 
  where
    portPairToWord16 :: (Word8, Word8) -> Word16
    portPairToWord16 = decode . runPut . put 

duplicateKey :: (Eq a) => (a, a) -> [(a, b)] -> [(a, b)]
duplicateKey (_from, _to) l = 
  case lookup _from l of
    Nothing -> l
    Just v -> (_to,v) : l


connectFor :: String -> IB -> OB -> IO ()
connectFor aID _i _o = do
  _i2 <- Stream.lockingInputStream _i
  _o2 <- Stream.lockingOutputStream _o

  let _io = catchIO ("connectFor " <> aID) - Stream.connect _i2 _o2

  _io

