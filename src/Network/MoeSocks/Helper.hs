{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}

module Network.MoeSocks.Helper where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Attoparsec.ByteString hiding (try)
import Data.Binary
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Text.Lens
import Data.Text.Strict.Lens (utf8)
import Network.MoeSocks.Internal.ShadowSocks.Encrypt
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import Prelude hiding (take, (-)) 
import System.IO.Unsafe (unsafePerformIO)
import System.Log.Logger
import System.Posix.IO (FdOption(CloseOnExec), setFdOption)
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB

-- BEGIN backports

infixr 0 -
(-) :: (a -> b) -> a -> b
(-) = ($)

-- END backports

_TBQueue_Size :: Int
_TBQueue_Size = 32

io :: (MonadIO m) => IO a -> m a
io = liftIO

flip4 :: (a, b, c, d) -> (d, c, b, a)
flip4 (_a, _b, _c, _d) = (_d, _c, _b, _a)

boolToMaybe :: Bool -> Maybe ()
boolToMaybe True = Just ()
boolToMaybe False = Nothing

syncLock :: MVar ()
syncLock = unsafePerformIO - newEmptyMVar

sync :: IO a -> IO a
sync aIO = do
  putMVar syncLock ()
  aIO <* takeMVar syncLock

puts :: String -> IO ()
puts = sync . debugM "moe" . ("😽  " <>)

pute :: String -> IO ()
pute = sync . errorM "moe" . ("😾  " <>)

putw :: String -> IO ()
putw = sync . warningM "moe" . ("😾  " <>)

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
  pure aID
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
catchExceptAsyncLog aID aIO = catches (() <$ aIO) 
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
catchIO aID aIO = catch (() <$ aIO) - \e ->
                pute - "IOError in " <> aID <> ": " 
                  <> show (e :: IOException)
                
logException :: String -> IO a -> IO ()
logException aID aIO = catch (() <$ aIO) - \e -> 
                        do
                          putw - "Logged error " <> aID <> ": " 
                            <> show (e :: SomeException)
                          throw e

wrapIO :: (Maybe String, IO c) -> IO ()
wrapIO (s,  _io) = do
  logException (fromMaybe "" s) _io 

waitBoth :: IO () -> IO () -> IO ()
waitBoth x y = do
  waitBothDebug (Nothing, x) (Nothing, y)

data WaitException = WaitException String

instance Show WaitException where
    show (WaitException s) = "Wait exception: " ++ s

instance Exception WaitException

waitBothDebug :: (Maybe String, IO ()) -> (Maybe String, IO ()) -> IO ()
waitBothDebug x y = do
  let _x = wrapIO x
      _y = wrapIO y

      _xID = x ^. _1 & fromMaybe ""
      _yID = y ^. _1 & fromMaybe ""
      _hID = _xID <> " / " <> _yID

  withAsync _x - \syncX -> do
    withAsync _y - \syncY -> do
      puts - "waiting for first: " <> _xID
      wait syncX 
      puts - "waiting for second: " <> _xID
      wait syncY
  
  pure ()

waitBothDebug' :: (Maybe String, IO ()) -> (Maybe String, IO ()) -> IO ()
waitBothDebug' x y = do
  let _x = wrapIO x
      _y = wrapIO y

      _xID = x ^. _1 & fromMaybe ""
      _yID = y ^. _1 & fromMaybe ""
      _hID = _xID <> " / " <> _yID

  let _init = mdo
        _threadXDone <- newEmptyMVar
        _threadYDone <- newEmptyMVar

        
        xThreadID <- forkFinally 
            (onException _x - (do
                                  puts - "onException: " 
                                          <> _xID
                                          <> ", throwTo thread: "
                                          <> _yID
                                  throwTo yThreadID - WaitException _xID
                              )) -
              const - putMVar _threadXDone ()

        yThreadID <- forkFinally
            (onException _y - (do
                                  puts - "onException: " 
                                          <> _yID
                                          <> ", throwTo thread: "
                                          <> _xID
                                  throwTo xThreadID - WaitException _yID
                              )) -
              const - putMVar _threadYDone ()

        return ((_threadXDone, xThreadID), (_threadYDone, yThreadID))

  let handleError ((_, xThreadID), (_, yThreadID)) = do
        pute - "handleError for " 
                <> _hID 
                <> ", killing thread: "
                <> _xID

        pure xThreadID
        pure yThreadID
        
        killThread xThreadID

        {-throwTo yThreadID - WaitException _yID-}
        {-throwTo xThreadID - WaitException _xID-}
        {-killThread xThreadID-}

  let action ((_threadXDone, _), (_threadYDone, _)) = do
        puts - "waiting for first: " <> _xID
        takeMVar _threadXDone 

        puts - "waiting for second: " <> _yID
        takeMVar _threadYDone
        puts - "All done for " <> _hID

  bracketOnError 
    _init
    handleError
    action

connectTunnel :: (Maybe String, IO ()) -> (Maybe String, IO ()) -> IO ()
connectTunnel x y = do
  let _x = wrapIO x
      _y = wrapIO y

      _xID = x ^. _1 & fromMaybe ""
      _yID = y ^. _1 & fromMaybe ""
      _hID = _xID <> " / " <> _yID

  withAsync _x - \syncX -> do
    withAsync _y - \syncY -> do
      puts - "waiting for first: " <> _xID
      wait syncX 
      puts - "waiting for second: " <> _xID
      wait syncY
  
  pure ()


-- connectTunnel only wait for the former. An exception is raised to
-- the latter after a coded time after the former has finished.
connectTunnel' :: (Maybe String, IO ()) -> (Maybe String, IO ()) -> IO ()
connectTunnel' x y = do
  let _x = wrapIO x
      _y = wrapIO y

      _xID = x ^. _1 & fromMaybe ""
      _yID = y ^. _1 & fromMaybe ""
      _hID = _xID <> " / " <> _yID

  let _init = mdo
        _threadXDone <- newEmptyMVar
        _threadYDone <- newEmptyMVar

        
        xThreadID <- forkFinally 
            (onException _x - (do
                                  puts - "onException: " 
                                          <> _xID
                                          <> ", throwTo thread: "
                                          <> _yID
                                  throwTo yThreadID - WaitException _xID
                              )) -
              const - putMVar _threadXDone ()

        yThreadID <- forkFinally
            (onException _y - (do
                                  puts - "onException: " 
                                          <> _yID
                                          <> ", throwTo thread: "
                                          <> _xID
                                  throwTo xThreadID - WaitException _yID
                              )) -
              const - putMVar _threadYDone ()

        return ((_threadXDone, xThreadID), (_threadYDone, yThreadID))

  let handleError ((_, xThreadID), (_, yThreadID)) = do
        pute - "handleError for " 
                <> _hID 
                <> ", killing thread: "
                <> _xID

        pure xThreadID
        pure yThreadID
        
        killThread xThreadID

        {-throwTo yThreadID - WaitException _yID-}
        {-throwTo xThreadID - WaitException _xID-}
        {-killThread xThreadID-}

  let action ((_threadXDone, _), (_threadYDone, yThreadID)) = do
        puts - "waiting for first: " <> _xID
        takeMVar _threadXDone 

        puts - "waiting for second: " <> _yID
        isEmptyMVar _threadYDone >>=
          flip when ( do
                          let oneSec = 1000000
                          threadDelay oneSec
                          isEmptyMVar _threadYDone >>=
                            flip when (throwTo yThreadID - 
                                        WaitException _yID
                                      )
                   )

        puts - "All done for " <> _hID

  bracketOnError 
    _init
    handleError
    action

connectProduction :: (Maybe String, IO ()) -> (Maybe String, IO ()) -> IO ()
connectProduction x y = do
  let _x = wrapIO x
      _y = wrapIO y

      _xID = x ^. _1 & fromMaybe ""
      _yID = y ^. _1 & fromMaybe ""
      _hID = _xID <> " / " <> _yID

  withAsync _x - \syncX -> do
    withAsync _y - \syncY -> do
      puts - "waiting for first: " <> _xID
      wait syncX 
      puts - "waiting for first: " <> _xID
      wait syncY
  
  pure ()

-- connectionProduction do not raise an exception on the latter when
-- an exception is raised on the former.
-- The proper termination of the latter is ensured by the logic
-- inside the former.
connectProduction' :: (Maybe String, IO ()) -> (Maybe String, IO ()) -> IO ()
connectProduction' x y = do
  let _x = wrapIO x
      _y = wrapIO y

      _xID = x ^. _1 & fromMaybe ""
      _yID = y ^. _1 & fromMaybe ""
      _hID = _xID <> " / " <> _yID

  let _init = mdo
        _threadXDone <- newEmptyMVar
        _threadYDone <- newEmptyMVar
        
        xThreadID <- forkFinally 
            (onException _x - (do
                                  puts - "onException: " 
                                          <> _xID
                                          <> ", throwTo thread: "
                                          <> _yID
                              )) -
              const - putMVar _threadXDone ()

        yThreadID <- forkFinally
            (onException _y - (do
                                  puts - "onException: " 
                                          <> _yID
                                          <> ", throwTo thread: "
                                          <> _xID
                                  throwTo xThreadID - WaitException _yID
                              )) -
              const - putMVar _threadYDone ()

        return ((_threadXDone, xThreadID), (_threadYDone, yThreadID))

  let handleError ((_, xThreadID), (_, yThreadID)) = do
        pute - "handleError for " 
                <> _hID 
                <> ", killing thread: "
                <> _xID

        pure xThreadID
        pure yThreadID
        
        killThread xThreadID


  let action ((_threadXDone, _), (_threadYDone, _)) = do
        puts - "waiting for first: " <> _xID
        takeMVar _threadXDone 

        puts - "waiting for second: " <> _yID
        takeMVar _threadYDone
        puts - "All done for " <> _hID

  bracketOnError 
    _init
    handleError
    action


getSocket :: (Integral i, Show i) => Text -> i -> SocketType ->
                                      IO (Socket, SockAddr)
getSocket aHost aPort aSocketType = do
    maybeAddrInfo <- firstOf folded <$>
                  getAddrInfo (Just hints) 
                              (Just - aHost ^. _Text) (Just - show aPort)

    case maybeAddrInfo of
      Nothing -> error - "Error in getSocket for: " <> aHost ^. _Text 
                              <> ":" <> show aPort
      Just addrInfo -> do
          let family     = addrFamily addrInfo
          let socketType = addrSocketType addrInfo
          let protocol   = addrProtocol addrInfo
          let address    = addrAddress addrInfo

          _socket <- socket family socketType protocol
          setSocketCloseOnExec _socket

          -- send immediately!
          setSocketOption _socket NoDelay 1 

          {-puts - "Getting socket: " <> show address-}

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


recv_ :: Socket -> IO ByteString
recv_ = flip recv 4096

send_ :: Socket -> ByteString -> IO ()
send_ = sendAll

sendBuilder :: TBQueue (Maybe ByteString) -> B.Builder -> IO ()
sendBuilder _queue = 
  atomically . writeTBQueue _queue . Just . builder_To_ByteString

sendBuilderEncrypted ::  TBQueue (Maybe ByteString) -> 
                          (ByteString -> IO ByteString) -> B.Builder -> IO ()
sendBuilderEncrypted _queue _encrypt x = 
  atomically . writeTBQueue _queue . Just =<< 
                                      _encrypt (builder_To_ByteString x)

-- | An exception raised when parsing fails.
data ParseException = ParseException String

instance Show ParseException where
    show (ParseException s) = "Parse exception: " ++ s

instance Exception ParseException

parseSocket :: String -> ByteString -> (ByteString -> IO ByteString) ->
                  Parser a -> Socket -> IO (ByteString, a)
parseSocket aID _partial _decrypt aParser = parseSocketWith aID - parse aParser
  where
    parseSocketWith :: String -> (ByteString -> Result a) ->
                        Socket -> IO (ByteString, a)
    parseSocketWith _id _parser _socket = do
      _rawBytes <- recv_ _socket
      {-puts - "rawBytes: " <> show _rawBytes-}
      _bytes <- _decrypt _rawBytes

      let r =  _parser - _partial <> _bytes
      case r of
        Done i _r -> pure (i, _r)
        Fail _ _ msg -> throwIO - ParseException -
                    "Failed to parse " <> _id <> ": " <> msg
        Partial _p -> parseSocketWith _id _p _socket

produceLoop :: String -> Socket -> TBQueue (Maybe ByteString) -> 
              (ByteString -> IO ByteString) -> IO ()
produceLoop aID aSocket aTBQueue f = do
  let _shutdown = tryIO aID - shutdown aSocket ShutdownReceive
      _produce = do
        _r <- recv_ aSocket 
        if (_r & isn't _Empty) 
          then do
            f _r >>= atomically . writeTBQueue aTBQueue . Just
            _produce 
          else do
            atomically - writeTBQueue aTBQueue Nothing

  _produce `onException` _shutdown
  pure ()

consumeLoop :: String -> Socket -> TBQueue (Maybe ByteString) -> IO ()
consumeLoop aID aSocket aTBQueue = do
  let _shutdown = tryIO aID - shutdown aSocket ShutdownSend
      _consume = do
        _r <- atomically - readTBQueue aTBQueue 
        case _r of
          Nothing -> _shutdown
          Just _data -> do
                          send_ aSocket _data >> _consume
  
  _consume `onException` _shutdown
  pure ()



-- Copied and slightly modified from: 
-- https://github.com/mzero/plush/blob/master/src/Plush/Server/Warp.hs
setSocketCloseOnExec :: Socket -> IO ()
setSocketCloseOnExec aSocket =
    setFdOption (fromIntegral $ fdSocket aSocket) CloseOnExec True


tryIO :: String -> IO a -> IO (Either IOException a)
tryIO _ = try -- . logException aID
