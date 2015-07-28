{-# LANGUAGE ExistentialQuantification #-}

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
import System.IO (hPutStrLn, stderr)
import System.IO.Streams (InputStream, OutputStream)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.ByteString.Lazy as LB
import qualified System.IO.Streams as Stream

-- BEGIN backports

infixr 0 -
(-) :: (a -> b) -> a -> b
(-) = ($)

-- END backports

type IB = InputStream ByteString
type OB = OutputStream ByteString

_Debug :: Bool
_Debug = False

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

_log :: String -> IO ()
_log = sync . putStrLn

puteT :: Text -> IO ()
puteT = pute . view _Text

showBytes :: ByteString -> String
showBytes = show . S.unpack

fromWord8 :: forall t. Binary t => [Word8] -> t
fromWord8 = decode . runPut . mapM_ put
      
logClose :: String -> Socket -> IO ()
logClose _ aSocket = do
      {-puts - "Closing socket " <> aID-}
      close aSocket 

logSocketWithAddress :: String -> IO (Socket, SockAddr) -> 
                        ((Socket, SockAddr) -> IO a) -> IO a
logSocketWithAddress aID _init f = do
  catch (bracket _init (logClose aID . fst) f) - \e -> do
      pute - "Exception in " <> aID <> ": " <> show (e :: SomeException)
      throw e

logSA:: String -> IO (Socket, SockAddr) -> 
                        ((Socket, SockAddr) -> IO a) -> IO a
logSA = logSocketWithAddress

logSocket :: String -> IO Socket -> (Socket -> IO a) -> IO a
logSocket aID _init f =
  catch (bracket _init (logClose aID) f) - \e -> do
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
                

wrapIO :: (Maybe String, IO c) -> IO ()
wrapIO (s,  _io) = do
  forM_ s - puts . ("+ " <>)
  _io
  forM_ s - puts . ("- " <>)

waitOneDebug :: (Maybe String, IO ()) -> (Maybe String, IO ()) -> IO () -> IO ()
waitOneDebug x y doneX = do
  waitY <- newEmptyMVar
  forkFinally (wrapIO y) -
                  const - putMVar waitY ()

  wrapIO x
  {-puts - "waitOneDebug: finalize"-}
  doneX
  {-puts - "waitOneDebug: waiting thread y"-}
  {-takeMVar waitY-}

  {-puts - "waitOneDebug: done thread y"-}

--    killThread yThreadID

{-waitBothDebug :: (Maybe String, IO ()) -> (Maybe String, IO ()) -> IO ()-}
{-waitBothDebug x y = do-}
  {-let-}
    {-initChildren :: IO (MVar [MVar ()])-}
    {-initChildren = newMVar []-}

    {-waitForChildren :: (MVar [MVar ()]) -> IO ()-}
    {-waitForChildren _children = do-}
     {-cs <- takeMVar _children-}
     {-case cs of-}
       {-[]   -> return ()-}
       {-m:ms -> do-}
          {-putMVar _children ms-}
          {-takeMVar m-}
          {-waitForChildren _children-}

    {-forkChild :: (MVar [MVar ()]) -> (Maybe String, IO ()) -> IO ThreadId-}
    {-forkChild _children io = do-}
       {-mvar <- newEmptyMVar-}
       {-childs <- takeMVar _children-}
       {-putMVar _children (mvar:childs)-}
       {-forkFinally (wrapIO io) (\_ -> putMVar mvar ())-}

    {-action _children = do-}
      {-forkChild _children x-}
      {-forkChild _children y-}
      {-waitForChildren _children-}

  {-bracket -}
    {-initChildren-}
    {-(const - pure ())-}
    {-action-}

{-waitBoth :: IO () -> IO () -> IO ()-}
{-waitBoth x y = do-}
  {-waitBothDebug (Nothing, x) (Nothing, y)-}
                

waitBoth :: IO () -> IO () -> IO ()
waitBoth x y = do
  waitOneDebug(Nothing, x) (Nothing, y) (pure ())
                
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
  getEncDec method (review utf8 password)


portNumber16 :: (Word8, Word8) -> Word16
portNumber16 pair = fromWord8 - toListOf both pair

duplicateKey :: (Eq a) => (a, a) -> [(a, b)] -> [(a, b)]
duplicateKey (_from, _to) l = 
  case lookup _from l of
    Nothing -> l
    Just v -> (_to,v) : l

setDone :: MVar () -> IO ()
setDone x = do
  {-puts - "setting Done!"-}
  {-putMVar x ()-}
  {-puts - "setting done complete"-}
  pure ()


connectFor :: MVar () -> IB -> OB -> IO ()
connectFor _doneFlag _i _o = do
  {-puts - "connecting"-}

  _i2 <- Stream.lockingInputStream _i
  _o2 <- Stream.lockingOutputStream _o

  let _io = catchIO - Stream.connect _i2 _o2

  _io

  {-_loopThreadID <- forkFinally _io - -}
                    {-const - setDone _doneFlag-}
  
  {-takeMVar _doneFlag-}

  {-[>puts - "killing loop"<]-}
  {-killThread _loopThreadID-}

  {-pure ()-}
