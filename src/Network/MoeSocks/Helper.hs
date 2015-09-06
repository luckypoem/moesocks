{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

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
import Data.Char (isUpper)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Text.Lens
import Data.Time.Clock
import Debug.Trace (trace)
import Network.MoeSocks.Internal.Socket (sendAllToFastOpen)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import Prelude hiding (take, (-)) 
import System.Log.Logger
import System.Posix.IO (FdOption(CloseOnExec), setFdOption)
import System.Random
import System.Timeout (timeout)
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Strict as S
import qualified Data.Text as T

-- BEGIN backports

infixr 0 -
{-# INLINE (-) #-}
(-) :: (a -> b) -> a -> b
f - x = f x 

-- END backports

is :: APrism s t a b -> s -> Bool
is x = not . isn't x

type HCipher = S.Maybe ByteString -> IO ByteString 
type HQueue = TBQueue (S.Maybe ByteString)

io :: (MonadIO m) => IO a -> m a
io = liftIO

flip4 :: (a, b, c, d) -> (d, c, b, a)
flip4 (_a, _b, _c, _d) = (_d, _c, _b, _a)

boolToMaybe :: Bool -> Maybe ()
boolToMaybe True = Just ()
boolToMaybe False = Nothing

trace' :: (Show a) => String -> a -> a
trace' aID x = trace ("TRACE " <> aID <> ": " <> show x) x

{-syncLock :: MVar ()-}
{-syncLock = unsafePerformIO - newEmptyMVar-}

{-sync :: IO a -> IO a-}
{-sync aIO = do-}
  {-[>putMVar syncLock ()<]-}
  {-[>aIO <* takeMVar syncLock<]-}
  {-aIO-}

debug_ :: String -> IO ()
debug_ = debugM "moe" 

error_ :: String -> IO ()
error_ = errorM "moe" 

warning_ :: String -> IO ()
warning_ = warningM "moe" 

info_ :: String -> IO ()
info_ = infoM "moe" 

notice_ :: String -> IO ()
notice_ = noticeM "moe" 

error_T :: Text -> IO ()
error_T = error_ . view _Text

showBytes :: ByteString -> String
showBytes = show . S.unpack

sleep :: Double -> IO ()
sleep x = threadDelay - floor - x * 1000 * 1000

foreverRun :: IO a -> IO ()
foreverRun _io = do
  forever - do
    _io
    debug_ "foreverRun delay 1 second"
    sleep 1

logClose :: String -> Socket -> IO ()
logClose aID aSocket = do
  pure aID
  debug_ - "Closing socket " <> aID
  close aSocket 


logSocketWithAddress :: String -> IO (Socket, SockAddr) -> 
                        ((Socket, SockAddr) -> IO a) -> IO a
logSocketWithAddress aID _init f = do
  catch (bracket _init (logClose aID . fst) f) - 
      \(e :: SomeException) -> do
      debug_ - "logSocket: Exception in " <> aID <> ": " <> show e
      throw e

logSA:: String -> IO (Socket, SockAddr) -> 
                        ((Socket, SockAddr) -> IO a) -> IO a
logSA = logSocketWithAddress

logSocket :: String -> IO Socket -> (Socket -> IO a) -> IO a
logSocket aID _init f =
  catch (bracket _init (logClose aID) f) - \e -> do
      debug_ - "Exception in " <> aID <> ": " <> show (e :: SomeException)
      throw e

catchExceptAsyncLog :: String -> IO a -> IO ()
catchExceptAsyncLog aID aIO = catches (() <$ aIO) 
                [ 
                  Handler - \(e :: AsyncException) -> do
                            error_ - "ASyncException in " 
                                    <> aID
                                    <> " : " <> show e
                            throw e
                , Handler - \(e :: SomeException) -> 
                            error_ - aID
                                    <> ": " <> show e
                ]

catchIO:: String -> IO a -> IO ()
catchIO aID aIO = catch (() <$ aIO) - \e ->
                error_ - "IOError in " <> aID <> ": " 
                  <> show (e :: IOException)
                
logException :: String -> IO a -> IO ()
logException aID aIO = catch (() <$ aIO) - \e -> 
                        do
                          debug_ - "Error in " <> aID <> ": " 
                            <> show (e :: SomeException)
                          throw e

logWaitIO :: (Maybe String, IO a) -> IO ()
logWaitIO x = do
  let _io = wrapIO x
      aID = x ^. _1 . _Just 

  debug_ - "waiting for : " <> aID
  _io
  
wrapIO :: (Maybe String, IO c) -> IO ()
wrapIO (s,  _io) = do
  logException (fromMaybe "" s) _io 

waitBoth :: IO () -> IO () -> IO ()
waitBoth x y = do
  waitBothDebug (Nothing, x) (Nothing, y)

data TimeoutException = TimeoutException String

instance Show TimeoutException where
    show (TimeoutException s) = "Timeout: " ++ s

instance Exception TimeoutException

waitBothDebug :: (Maybe String, IO ()) -> (Maybe String, IO ()) -> IO ()
waitBothDebug x y = do
  concurrently (logWaitIO x) (logWaitIO y)
  let _xID = x ^. _1 . _Just 
      _yID = y ^. _1 . _Just
      _hID = _xID <> " / " <> _yID
  debug_ - "All done for " <> _hID
  pure ()

connectTunnel :: (Maybe String, IO ()) -> (Maybe String, IO ()) -> IO ()
connectTunnel x y = 
  let _prolong _io = _io >> sleep 10
  in

  race_ (_prolong - logWaitIO x) 
        (_prolong - logWaitIO y)

connectMarket :: (Maybe String, IO ()) -> (Maybe String, IO ()) -> IO ()
connectMarket = waitBothDebug

getSocket :: (Integral i, Show i) => Text -> i -> SocketType ->
                                      IO (Socket, SockAddr)
getSocket = getSocketWithHint AF_UNSPEC

getSocketWithHint :: (Integral i, Show i) => 
                        Family -> Text -> i -> SocketType -> 
                        IO (Socket, SockAddr)
getSocketWithHint aFamily aHost aPort aSocketType = do
    debug_ - "getSocketWithHint: " <> show aFamily <> 
              " " <> show aHost <> ":" <> show aPort

    _addrs <- getAddrInfo (Just hints) 
                              (Just - aHost ^. _Text) (Just - show aPort)

    debug_ - "Get socket addrs: " <> show _addrs

    maybeAddrInfo <- firstOf folded <$> pure _addrs

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
          when (aSocketType == Stream) -
            setSocketOption _socket NoDelay 1 

          debug_ - "Got socket: " <> show addrInfo
          {-debug_ - "Socket family: " <> show family-}
          {-debug_ - "Socket protocol: " <> show protocol -}

          pure (_socket, address)
          
  where
    hints = defaultHints 
            {
              addrFlags = [AI_ADDRCONFIG, AI_NUMERICSERV]
            , addrSocketType = aSocketType
            , addrFamily = aFamily
            }

builder_To_ByteString :: B.Builder -> ByteString
builder_To_ByteString = LB.toStrict . B.toLazyByteString

portPairToInt :: (Word8, Word8) -> Int
portPairToInt = fromIntegral . portPairToWord16 
  where
    portPairToWord16 :: (Word8, Word8) -> Word16
    portPairToWord16 = decode . runPut . put 


recv_ :: Socket -> IO ByteString
recv_ = flip recv 4096

send_ :: Socket -> ByteString -> IO ()
send_ = sendAll

sendFast :: Socket -> ByteString -> SockAddr -> IO ()
sendFast = sendAllToFastOpen


sendAllRandom :: Int -> Socket -> ByteString -> IO ()
sendAllRandom aFlushBound aSocket aBuffer = do
  _loop aBuffer
  where
    _loop _buffer = do
      _randomLength <- randomRIO (0, aFlushBound)
      {-notice_ - "randomLength: " <> show _randomLength-}
      let (_thisBuffer, _nextBuffer) = S.splitAt _randomLength _buffer
      sendAll aSocket _thisBuffer
      when (_nextBuffer & isn't _Empty) - do
        yield
        _loop _nextBuffer

sendBytes :: HQueue -> ByteString -> IO ()
sendBytes _queue = atomically . writeTBQueue _queue . S.Just

sendBytesEncrypt :: HQueue -> HCipher -> ByteString -> IO ()
sendBytesEncrypt _queue _cipher x = sendBytes _queue =<< _cipher (S.Just x)

sendBuilder :: HQueue -> B.Builder -> IO ()
sendBuilder _queue = sendBytes _queue . builder_To_ByteString

sendBuilderEncrypt :: HQueue -> HCipher -> B.Builder -> IO ()
sendBuilderEncrypt _queue _encrypt = 
  sendBytesEncrypt _queue _encrypt . builder_To_ByteString

-- | An exception raised when parsing fails.
data ParseException = ParseException String

instance Show ParseException where
    show (ParseException s) = "Parse exception: " <> s

instance Exception ParseException

type Timeout = Int

timeoutFor :: String -> Timeout -> IO a -> IO a
timeoutFor aID aTimeout aIO = do
  timeout aTimeout aIO >>= \case
    Nothing -> throw - TimeoutException aID
    Just _r -> pure _r

parseSocket :: String -> ByteString -> HCipher ->
                  Parser a -> Socket -> IO (ByteString, a)
parseSocket aID _partial _decrypt aParser aSocket = do
  let _timeout = 5 * 1000 * 1000 -- 5 secs
  timeoutFor aID _timeout - parseSocketWith aID (parse aParser) aSocket

  where
    parseSocketWith :: String -> (ByteString -> Result a) ->
                        Socket -> IO (ByteString, a)
    parseSocketWith _id _parser _socket = do
      _rawBytes <- recv_ _socket
      {-debug_ - "rawBytes: " <> show _rawBytes-}
      _bytes <- _decrypt (S.Just _rawBytes)

      case _parser - _partial <> _bytes of
        Done i _r -> pure (i, _r)
        Fail _ _ msg -> throwIO - ParseException -
                    "Failed to parse " <> _id <> ": " <> msg
        Partial _p -> parseSocketWith _id _p _socket


-- throttle speed in kilobytes per second
produceLoop :: String -> Timeout -> Maybe Double ->
              Socket -> HQueue -> 
              HCipher -> IO ()
produceLoop aID aTimeout aThrottle aSocket aTBQueue f = do
  _startTime <- getCurrentTime

  let _shutdown = do
                    tryIO aID - shutdown aSocket ShutdownReceive
                  
      _produce :: Int -> IO ()
      _produce _bytesReceived = flip onException (f S.Nothing) - do
        _r <- timeoutFor aID aTimeout - recv_ aSocket
        {-when ("L" `isPrefixOf` aID) - do-}
          {-notice_ - "Get chunk: " <> (show - S.length _r) <> " " <> aID-}
        
        if (_r & isn't _Empty) 
          then do
            forM_ aThrottle - \_throttle -> do
              _currentTime <- getCurrentTime
              let _timeDiff = realToFrac (diffUTCTime _currentTime 
                                                      _startTime) :: Double

                  _bytesK = fromIntegral _bytesReceived / 1000 :: Double

              let _speed = _bytesK / _timeDiff

              debug_ - "Produce speed is: " <> show (floor _speed :: Int) <> " K"

              when (_speed > _throttle) - do
                let _sleepTime = ((_bytesK + (-_timeDiff * _throttle))
                                      / _throttle ) * 1000 * 1000

                debug_ - "Produce sleeping for: " <> show _sleepTime 
                        <> " miliseconds."
                threadDelay - floor - _sleepTime
              
            f (S.Just _r) >>= atomically . writeTBQueue aTBQueue . S.Just
            yield
            _produce (_bytesReceived + S.length _r)
          else do
            debug_ -  "Half closed: " <> aID 
            f S.Nothing >>= atomically . writeTBQueue aTBQueue . S.Just
            atomically - writeTBQueue aTBQueue S.Nothing

  _produce 0 `onException` _shutdown
  pure ()

  

consumeLoop :: String -> Timeout -> Maybe Double ->
                Socket -> HQueue -> Bool -> Int -> IO ()
consumeLoop aID aTimeout aThrottle aSocket aTBQueue randomize aBound = do
  _startTime <- getCurrentTime
  
  
  let _shutdown = do
                    tryIO aID - shutdown aSocket ShutdownSend

      _consume :: Int -> IO ()
      _consume _allBytesSent = do
        forM_ aThrottle - \_throttle -> do
          _currentTime <- getCurrentTime
          let _timeDiff = realToFrac (diffUTCTime _currentTime 
                                                  _startTime) :: Double

              _bytesK = fromIntegral _allBytesSent / 1000 :: Double

          let _speed = _bytesK / _timeDiff

          debug_ - "Consume speed is: " <> show (floor _speed :: Int) <> " K"

          when (_speed > _throttle) - do
            let _sleepTime = ((_bytesK + (-_timeDiff * _throttle))
                                  / _throttle ) * 1000 * 1000

            debug_ - "Consume sleeping for: " <> show _sleepTime 
                    <> " miliseconds."
            threadDelay - floor - _sleepTime

        
                            
        _newPacket <- atomically - readTBQueue aTBQueue

        case _newPacket of
          S.Nothing -> () <$ _shutdown
          S.Just _data -> do
                          let _send = if randomize 
                                          then sendAllRandom aBound
                                          else sendAll
                                                    
                          timeoutFor aID aTimeout - 
                                          _send aSocket _data
                          yield
                          _consume - 
                            _allBytesSent + S.length _data
  
  _consume 0 `onException` _shutdown
  pure ()


setSocket_TCP_FAST_OPEN:: Socket -> IO ()
setSocket_TCP_FAST_OPEN aSocket = 
  let _TCP_FASTOPEN = 23
      _TCP_Option = 6
  in
  setSocketOption aSocket (CustomSockOpt (_TCP_Option, _TCP_FASTOPEN)) 1 

setSocket_TCP_NOTSENT_LOWAT :: Socket -> IO ()
setSocket_TCP_NOTSENT_LOWAT aSocket = 
  let _TCP_NOTSENT_LOWAT = 25
      _TCP_Option = 6
  in
  setSocketOption aSocket (CustomSockOpt (_TCP_Option, _TCP_NOTSENT_LOWAT)) 1 


-- Copied and slightly modified from: 
-- https://github.com/mzero/plush/blob/master/src/Plush/Server/Warp.hs
setSocketCloseOnExec :: Socket -> IO ()
setSocketCloseOnExec aSocket =
    setFdOption (fromIntegral - fdSocket aSocket) CloseOnExec True


tryIO :: String -> IO a -> IO (Either IOException a)
tryIO _ = try -- . logException aID

toHaskellNamingConvention :: Text -> Text
toHaskellNamingConvention x = 
  let xs = x & T.split (`elem` ['_', '-'])
  in
  if xs & anyOf each (T.all isUpper)
    then x
    else xs
            & over (_tail . traversed) T.toTitle
            & T.concat
