{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.MoeSocks.App where

import Control.Concurrent.Async hiding (waitBoth)
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader hiding (local)
import Control.Monad.Writer hiding (listen)
import Data.Text.Lens
import Network.MoeSocks.Encrypt (safeMethods, unsafeMethods)
import Network.MoeSocks.Helper
import Network.MoeSocks.Modules.Resource (loadConfig)
import Network.MoeSocks.Runtime
import Network.MoeSocks.Type
import Network.MoeSocks.Type.Bootstrap.Option
import Network.MoeSocks.Handler
import Prelude hiding ((-), take)

withGateOptions :: Options -> IO a -> IO ()
withGateOptions aOption aIO = do
  if aOption ^. listMethods
    then do
      let _br = putStrLn ""

      _br
      putStrLn "Recommended:"
      itraverse_ (\k _ -> putStrLn - "\t\t" <> k ^. _Text) - safeMethods

      _br
      putStrLn "Supported:"
      itraverse_ (\k _ -> putStrLn - "\t\t" <> k ^. _Text) - unsafeMethods

    else
      () <$ aIO

runJob :: Env -> Job -> TVar JobStatus -> IO ()
runJob aEnv (RemoteRelayJob x) _ = runRemoteRelay aEnv x
runJob aEnv (LocalServiceJob x) _ = runLocalService aEnv x

runApp :: Env -> [Job] -> IO ()
runApp aEnv someJobs = do
  _jobs <- (forM someJobs - \_job -> (,) _job
                                      <$> newTVarIO initialJobStatus)
  _asyncs <- mapM (async . foreverRun . (uncurry (runJob aEnv))) _jobs

  _mainThread <- async - do
    waitAnyCancel - _asyncs

  _uiThread <- async - forever - do
    let _statuses = _jobs ^.. each . _2 :: [TVar JobStatus]
    {-forM_ _statuses - readTVarIO >=> print-}
    sleep 5

  waitAnyCancel [_mainThread, _uiThread]

  pure ()


moeApp:: MoeMonadT ()
moeApp = do
  _options <- ask
  _config <- lift - loadConfig - _options

  (_runtime, _jobs) <- lift - initRuntime _config _options 

  io - runApp (_runtime ^. env) _jobs
