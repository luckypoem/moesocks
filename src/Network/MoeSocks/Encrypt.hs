-- Heavily inspired by
-- https://github.com/rnons/shadowsocks-haskell/blob/86bd47d474df6800e1f3706b33ae9e8f80696d8f/Shadowsocks/Encrypt.hs


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.MoeSocks.Encrypt where

import Control.Concurrent.MVar (newEmptyMVar, isEmptyMVar, putMVar, readMVar)
import Control.Lens
import Control.Monad.Except
import Crypto.Hash.MD5 (hash)
import Data.ByteString (ByteString)
import Data.Map
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Strict.Lens (utf8)
import Data.Text.Lens
import OpenSSL (withOpenSSL)
import OpenSSL.EVP.Cipher (getCipherByName, CryptoMode(..))
import qualified OpenSSL.EVP.Internal as E
import OpenSSL.Random (randBytes)
import qualified Data.ByteString as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Strict as S
import Prelude hiding ((-))

-- BEGIN backports

infixr 0 -
(-) :: (a -> b) -> a -> b
(-) = ($)

-- END backports

type KeyLength = Int
type IV_Length = Int

methods :: Map Text (KeyLength, IV_Length)
methods = fromList
    [ ("aes-128-cfb", (16, 16))
    , ("aes-192-cfb", (24, 16))
    , ("aes-256-cfb", (32, 16))
    , ("bf-cfb", (16, 8))
    , ("camellia-128-cfb", (16, 16))
    , ("camellia-192-cfb", (24, 16))
    , ("camellia-256-cfb", (32, 16))
    , ("cast5-cfb", (16, 8))
    , ("des-cfb", (8, 8))
    , ("idea-cfb", (16, 8))
    , ("rc2-cfb", (16, 8))
    , ("rc4", (16, 0))
    , ("seed-cfb", (16, 16))
    ]


hashKey :: ByteString -> Int -> Int -> ByteString
hashKey aPassword aKeyLen a_IV_len = loop mempty mempty 
  where
    _stopLength = aKeyLen + a_IV_len

    loop :: ByteString -> ByteString -> ByteString
    loop _lastHashedBytes _accumHashedBytes
      | S.length _accumHashedBytes >= _stopLength = S.take aKeyLen
                                                            _accumHashedBytes
      | otherwise = let _new = hash $ _lastHashedBytes <> aPassword
                    in
                    loop _new $ _accumHashedBytes <> _new

type Cipher = S.Maybe ByteString -> IO ByteString 

ssl :: IO a -> IO a
ssl = withOpenSSL

type CipherBuilder = ByteString -> IO Cipher

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x


type MaybeExceptT = ExceptT () IO

getMaybe :: Maybe a -> MaybeExceptT a
getMaybe Nothing = throwError ()
getMaybe (Just x) = pure x

mio :: IO (Maybe a) -> MaybeExceptT a
mio = (>>= getMaybe) . io

io :: (MonadIO m) => IO a -> m a
io = liftIO

type CipherBox = (Int, IO ByteString, CipherBuilder, CipherBuilder)

plainCipher :: Cipher
plainCipher = pure . S.fromMaybe mempty

initBuilder :: Text -> Text -> IO (Maybe CipherBox)
initBuilder aMethod aPassword 
  | aMethod == "none" = let constCipher = const - pure plainCipher
                        in
                        pure - Just - (0, pure mempty, constCipher, constCipher)

  | otherwise =
      fmap eitherToMaybe - runExceptT - initBuilder' aMethod aPassword

initBuilder' :: Text -> Text -> MaybeExceptT CipherBox
initBuilder' aMethod aPassword = do
  _method <- mio - ssl - getCipherByName - aMethod ^. _Text

  (_keyLength, _IV_Length) <- getMaybe - methods ^? ix aMethod

  let _hashed = hashKey (review utf8 aPassword) _keyLength _IV_Length
      _IV_Maker = io - ssl - randBytes _IV_Length

      _encryptBuilder :: ByteString -> IO Cipher
      _encryptBuilder _iv = do
          _ctx <- io - ssl - E.cipherInitBS _method _hashed _iv Encrypt

          let _encrypt :: Cipher
              _encrypt = \case
                  S.Nothing -> E.cipherFinalBS _ctx
                  S.Just _bytes -> do
                    ssl - E.cipherUpdateBS _ctx _bytes

          pure _encrypt

      _decryptBuilder :: ByteString -> IO Cipher
      _decryptBuilder _iv = do
          _ctx <- io - ssl - E.cipherInitBS _method _hashed _iv Decrypt

          let _decrypt :: Cipher
              _decrypt = \case
                  S.Nothing -> E.cipherFinalBS _ctx
                  S.Just _bytes -> do
                    ssl - E.cipherUpdateBS _ctx _bytes 
          
          pure _decrypt
          

  pure - (_IV_Length, _IV_Maker, _encryptBuilder, _decryptBuilder)

