{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.MoeSocks.Modules.Resource where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer hiding (listen)
import Data.Aeson hiding (Result)
import Data.Aeson.Lens
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lens
import Network.MoeSocks.Config
import Network.MoeSocks.Helper
import Network.MoeSocks.Type
import Prelude hiding ((-), take)
import qualified Data.HashMap.Strict as H
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

loadConfig :: Options -> MoeMonadT Config
loadConfig aOption = do
  let _maybeFilePath = aOption ^. configFile 

  _v <- case _maybeFilePath of
          Nothing -> pure - Just - Object mempty
          Just _filePath -> fmap (preview _JSON) - 
                            io - TIO.readFile - _filePath ^. _Text

  let 

      asList :: ([(Text, Value)] -> [(Text, Value)]) -> Value -> Value
      asList f = over _Object - H.fromList . f . H.toList 
      
      fromShadowSocksConfig :: Text -> Text
      fromShadowSocksConfig x = 
        let fixes = Map.fromList
              [
                ("server", "remoteAddress")
              , ("server_port", "remotePort")
              , ("local_address", "localAddress")
              , ("local_port", "localPort")
              ]

        in
        fixes ^? ix x & fromMaybe x

      toParsableConfig :: Value -> Value
      toParsableConfig = asList - each . _1 %~  (
                                                  T.cons '_' 
                                                {-. toHaskellNamingConvention-}
                                                . fromShadowSocksConfig
                                                )  

      toReadableConfig :: Value -> Value
      toReadableConfig = asList - each . _1 %~ T.tail 

      showConfig :: Config -> Text
      showConfig =  review _JSON
                    . toReadableConfig 
                    . review _JSON 

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
      
      _maybeConfig = -- trace ("JSON: " <> show _v) 
                      _v
                      >>= decode 
                          . encode 
                          . fallbackConfig optionalConfig
                          . insertParams (aOption ^. params)
                          . toParsableConfig 

  case _maybeConfig of
    Nothing -> do
      let _r = 
            execWriter - do
              tell "\n\n"
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

      throwError - _r ^. _Text 

    Just _config -> do
      let configStr = showConfig _config ^. _Text :: String
      io - debug_ - "Using config: " <> configStr
      pure - _config 
