{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.MoeSocks.Bootstrap where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer hiding (listen)
import Data.Aeson hiding (Result)
import Data.Aeson.Lens
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lens
import Network.MoeSocks.Default
import Network.MoeSocks.Encrypt (safeMethods, unsafeMethods)
import Network.MoeSocks.Helper
import Prelude hiding ((-), take)
import qualified Data.HashMap.Strict as H
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.MoeSocks.Type.Bootstrap.Config as C
import qualified Network.MoeSocks.Type.Bootstrap.Option as O


asList :: ([(Text, Value)] -> [(Text, Value)]) -> Value -> Value
asList f = over _Object - H.fromList . f . H.toList

toReadableConfig :: Value -> Value
toReadableConfig = asList - each . _1 %~ T.tail

showConfig :: C.Config -> Text
showConfig =  review _JSON
              . toReadableConfig
              . review _JSON

withGateOptions :: O.Options -> IO a -> IO ()
withGateOptions someOptions aIO = do
  let _br = putStrLn ""

  if someOptions ^. O.listMethods
    then do
      _br
      putStrLn "Recommended:"
      itraverse_ (\k _ -> putStrLn - "\t\t" <> k ^. _Text) - safeMethods

      _br
      putStrLn "Supported:"
      itraverse_ (\k _ -> putStrLn - "\t\t" <> k ^. _Text) - unsafeMethods

    else
      if someOptions ^. O.showDefaultConfig
        then
          putStrLn - showConfig defaultConfig ^. _Text
        else
          () <$ aIO

loadConfig :: O.Options -> ExceptT String IO C.Config
loadConfig someOptions = do
  let _maybeFilePath = someOptions ^. O.configFile

  _v <- case _maybeFilePath of
          Nothing -> pure - Just - Object mempty
          Just _filePath -> fmap (preview _JSON) -
                            io - TIO.readFile - _filePath ^. _Text

  let

      fixConfig:: Text -> Text
      fixConfig x =
        let _remoteHost = "remoteHost"
            _remotePort = "remotePort"
            _localHost = "localHost"

            fixes = Map.fromList
              [
                ("server"       , _remoteHost )
              , ("remote"       , _remoteHost )
              , ("remoteAddress", _remoteHost )

              , ("server_port"  , _remotePort )

              , ("local"        , _localHost  )
              , ("localAddress" , _localHost  )
              , ("local_address", _localHost  )

              ]

        in
        fixes ^? ix x & fromMaybe x

      toParsableConfig :: Value -> Value
      toParsableConfig = asList - each . _1 %~  (
                                                  T.cons '_'
                                                . toHaskellNamingConvention
                                                . fixConfig
                                                )

      filterEssentialConfig :: Value -> Value
      filterEssentialConfig = over _Object - \_obj ->
                                foldl (flip H.delete) _obj -
                                  [
                                    "_password"
                                  ]

      insertConfig :: Value -> Value -> Value
      insertConfig (Object _from) = over _Object (_from `H.union`)
      insertConfig _ = const Null

      insertParams :: [(Text, Value)] -> Value -> Value
      insertParams _from = over _Object (H.fromList _from `H.union`)

      fallbackConfig :: Value -> Value -> Value
      fallbackConfig = flip insertConfig

      optionalConfig = filterEssentialConfig - toJSON defaultConfig

      _maybeConfig =  -- trace' "JSON: "
                      _v
                      >>= decode
                          . encode
                          . fallbackConfig optionalConfig
                          . insertParams (someOptions ^. O.params)
                          . toParsableConfig

  case _maybeConfig of
    Nothing -> do
      let _r =
            execWriter - do
              tell "\n"
              case _maybeFilePath of
                Just _filePath -> do
                                    tell "Failed to parse configuration file: "
                                    tell _filePath
                                    tell "\n"
                                    tell "Example: \n"
                                    tell - showConfig defaultConfig <> "\n"
                Nothing -> do
                            tell "The password argument '-k' is required.\n"
                            tell "Alternatively, use '-c' to provide a "
                            tell "configuration file.\n"
                            tell "\n"
                            tell "Use '-h' to show help text"

              tell "\n"

      throwError - _r ^. _Text

    Just _config -> do
      let configStr = showConfig _config ^. _Text :: String
      io - debug_ - "Using config: " <> configStr
      pure - _config
