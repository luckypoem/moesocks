{-# LANGUAGE ForeignFunctionInterface #-}

module Network.MoeSocks.Internal.Socket where

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Monoid
import Foreign.C.Types
import Foreign.Ptr (Ptr)
import Network.Socket
import Network.Socket.ByteString
import Network.Socket.Internal (throwSocketErrorWaitWrite, withSockAddr)
import qualified Data.ByteString as S
import qualified Data.Strict as S

foreign import ccall unsafe "sendto" c_sendto ::
  CInt -> Ptr a -> CSize -> CInt -> Ptr SockAddr -> CInt -> IO CInt

-- | Send data to the socket.  The recipient can be specified
-- explicitly, so the socket need not be in a connected state.
-- Returns the number of bytes sent.  Applications are responsible for
-- ensuring that all data has been sent.
sendBufToWithFlag :: Socket            -- (possibly) bound/connected Socket
          -> Ptr a -> Int  -- Data to send
          -> SockAddr
          -> Int
          -> IO Int            -- Number of Bytes sent
sendBufToWithFlag 
  sock@(MkSocket s _family _stype _protocol _status) ptr nbytes addr flags = do
   withSockAddr addr $ \p_addr sz -> do
    {-debug_ - "sendto flag: " <> show flags-}
    liftM fromIntegral $
      throwSocketErrorWaitWrite sock "sendTo" $
        c_sendto s ptr (fromIntegral $ nbytes) (fromIntegral flags)
                          p_addr (fromIntegral sz)

sendToWithFlag :: Socket      -- ^ Socket
       -> ByteString  -- ^ Data to send
       -> SockAddr    -- ^ Recipient address
       -> Int
       -> IO Int      -- ^ Number of bytes sent
sendToWithFlag sock xs addr flags =
    unsafeUseAsCStringLen xs $ \(str, len) -> 
      sendBufToWithFlag sock str len addr flags

sendAllFastOpenTo :: Socket      -- ^ Socket
          -> ByteString  -- ^ Data to send
          -> SockAddr    -- ^ Recipient address
          -> IO ()
sendAllFastOpenTo sock xs addr = do
    {-debug_ - "sendFast " <> show addr <> ": " <> show xs-}
    let _MSG_FASTOPEN  = 0x20000000  
    sent <- sendToWithFlag sock xs addr _MSG_FASTOPEN
    when (sent < S.length xs) $ sendAllTo sock (S.drop sent xs) addr
