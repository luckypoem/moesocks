-- Heavily inspired by
-- https://github.com/rnons/shadowsocks-haskell/blob/86bd47d474df6800e1f3706b33ae9e8f80696d8f/Shadowsocks/Encrypt.hs


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.MoeSocks.Encrypt 
(
  initCipherBox
, identityCipher
, ssl
)
where

import Control.Lens
import Control.Monad.Except
import Crypto.Hash.MD5 (hash)
import Data.ByteString (ByteString)
import Data.Map
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Lens
import Data.Text.Strict.Lens (utf8)
import OpenSSL (withOpenSSL)
import OpenSSL.EVP.Cipher (getCipherByName, CryptoMode(..))
import OpenSSL.Random (randBytes)
import Prelude hiding ((-))
import qualified Data.ByteString as S
import qualified Data.Strict as S
import qualified OpenSSL.EVP.Internal as E

-- BEGIN backports

infixr 0 -
{-# INLINE (-) #-}
(-) :: (a -> b) -> a -> b
(-) = ($)

-- END backports

type KeyLength = Int
type IV_Length = Int

type Cipher = S.Maybe ByteString -> IO ByteString 
type IV = ByteString
type CipherBuilder = IV -> IO Cipher

type EncryptBuilder = CipherBuilder
type DecryptBuilder = CipherBuilder

type CipherBox = (IV_Length, IO IV, EncryptBuilder, DecryptBuilder)

type MaybeT_IO = ExceptT () IO

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


hashKey :: ByteString -> KeyLength -> IV_Length -> ByteString
hashKey aPassword aKeyLen a_IV_len = loop mempty mempty 
  where
    _stopLength = aKeyLen + a_IV_len

    loop :: ByteString -> ByteString -> ByteString
    loop _lastHashedBytes _accumHashedBytes
      | S.length _accumHashedBytes >= _stopLength 
        = S.take aKeyLen _accumHashedBytes
      | otherwise = let _new = hash - _lastHashedBytes <> aPassword
                    in
                    loop _new - _accumHashedBytes <> _new


ssl :: IO a -> IO a
ssl = withOpenSSL

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

getMaybe :: Maybe a -> MaybeT_IO a
getMaybe Nothing = throwError ()
getMaybe (Just x) = pure x

mio :: IO (Maybe a) -> MaybeT_IO a
mio = (>>= getMaybe) . liftIO

identityCipher :: Cipher
identityCipher = pure . S.fromMaybe mempty

initCipherBox :: Text -> Text -> IO (Maybe CipherBox)
initCipherBox aMethod aPassword 
  | aMethod == "none" = let constCipher = const - pure identityCipher
                        in
                        pure - Just (0, pure mempty, constCipher, constCipher)

  | otherwise = ssl - 
      fmap eitherToMaybe - runExceptT - initCipherBox' aMethod aPassword

initCipherBox' :: Text -> Text -> MaybeT_IO CipherBox
initCipherBox' aMethod aPassword = do
  _method <- mio - getCipherByName - aMethod ^. _Text

  (_keyLength, _IV_Length) <- getMaybe - methods ^? ix aMethod

  let _hashed = hashKey (review utf8 aPassword) _keyLength _IV_Length
      _IV_Maker = randBytes _IV_Length

      _encryptBuilder :: CipherBuilder
      _encryptBuilder _iv = do
          _ctx <- E.cipherInitBS _method _hashed _iv Encrypt

          let _encrypt :: Cipher
              _encrypt = \case
                  S.Nothing -> E.cipherFinalBS _ctx
                  S.Just _bytes -> do
                    E.cipherUpdateBS _ctx _bytes

          pure _encrypt

      _decryptBuilder :: CipherBuilder 
      _decryptBuilder _iv = do
          _ctx <- E.cipherInitBS _method _hashed _iv Decrypt

          let _decrypt :: Cipher
              _decrypt = \case
                  S.Nothing -> E.cipherFinalBS _ctx
                  S.Just _bytes -> do
                    E.cipherUpdateBS _ctx _bytes 
          
          pure _decrypt
          

  pure - (_IV_Length, _IV_Maker, _encryptBuilder, _decryptBuilder)

