{- Original file from the shadowsocks package hosted on Hackage:
 - https://hackage.haskell.org/package/shadowsocks
 - Copyright: rnons
 - Licence: MIT
 - Slightly modified to work better with moesocks
 -}


{- 
The MIT License (MIT)

Copyright (c) 2014 rnons

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-}



{-# LANGUAGE OverloadedStrings #-}
module Network.MoeSocks.Internal.ShadowSocks.Encrypt
  ( getEncDec
  , iv_len
  ) where

import           Control.Concurrent.MVar ( newEmptyMVar, isEmptyMVar
                                         , putMVar, readMVar)
import           Crypto.Hash.MD5 (hash)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           OpenSSL (withOpenSSL)
import           OpenSSL.EVP.Cipher (getCipherByName, CryptoMode(..))
import           OpenSSL.EVP.Internal (cipherInitBS, cipherUpdateBS)
import           OpenSSL.Random (randBytes)
import           Data.Text (Text)
import           Control.Lens
import           Data.Text.Lens


method_supported :: HM.HashMap Text (Int, Int)
method_supported = HM.fromList
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

iv_len :: Text -> Int
iv_len method = m1
  where
    (_, m1) = method_supported HM.! method

evpBytesToKey :: ByteString -> Int -> Int -> (ByteString, ByteString)
evpBytesToKey password keyLen ivLen =
    let ms' = S.concat $ ms 0 []
        key = S.take keyLen ms'
        iv  = S.take ivLen $ S.drop keyLen ms'
     in (key, iv)
  where
    ms :: Int -> [ByteString] -> [ByteString]
    ms 0 _ = ms 1 [hash password]
    ms i m
        | S.length (S.concat m) < keyLen + ivLen =
            ms (i+1) (m ++ [hash (last m <> password)])
        | otherwise = m

getSSLEncDec :: Text -> ByteString
             -> IO (ByteString -> IO ByteString, ByteString -> IO ByteString)
getSSLEncDec method password = do
    let (m0, m1) = fromJust $ HM.lookup method method_supported
    random_iv <- withOpenSSL $ randBytes 32
    let cipher_iv = S.take m1 random_iv
    let (key, _) = evpBytesToKey password m0 m1
    cipherCtx <- newEmptyMVar
    decipherCtx <- newEmptyMVar

    cipherMethod <- fmap fromJust $ withOpenSSL $ getCipherByName $ 
                                        method ^. _Text
    ctx <- withOpenSSL $ cipherInitBS cipherMethod key cipher_iv Encrypt
    let
        encrypt buf = 
          if S.null buf
            then return $! S.empty
            else do
              empty <- isEmptyMVar cipherCtx
              if empty
                  then do
                      putMVar cipherCtx $! ()
                      ciphered <- withOpenSSL $ cipherUpdateBS ctx buf
                      return $! cipher_iv <> ciphered
                  else do
                      r <- withOpenSSL $ cipherUpdateBS ctx buf
                      return $! r

        decrypt buf =
          if S.null buf
            then return $! S.empty
            else do
              empty <- isEmptyMVar decipherCtx
              if empty
                  then do
                      let decipher_iv = S.take m1 buf
                      dctx <- withOpenSSL $ 
                              cipherInitBS cipherMethod key decipher_iv Decrypt
                      putMVar decipherCtx $! dctx
                      if S.null (S.drop m1 buf)
                          then return ""
                          else do
                              r <- withOpenSSL $
                                      cipherUpdateBS dctx (S.drop m1 buf)
                              return $! r
                  else do
                      dctx <- readMVar decipherCtx
                      r <- withOpenSSL $ cipherUpdateBS dctx buf
                      return $! r

    return (encrypt, decrypt)

getEncDec :: Text -> ByteString  
          -> IO (ByteString -> IO ByteString, ByteString -> IO ByteString)
getEncDec t
  | t == "none" = const $ pure (pure, pure)
  | otherwise = getSSLEncDec t
