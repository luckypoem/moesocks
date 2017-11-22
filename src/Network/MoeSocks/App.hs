{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.MoeSocks.App where

import           Control.Concurrent.Async hiding (waitBoth)
import           Control.Lens

import           Network.MoeSocks.Bootstrap (loadConfig)
import           Network.MoeSocks.Handler (runRemoteRelay, runLocalService)
import           Network.MoeSocks.Helper ((-), io, error_, foreverRun)
import           Network.MoeSocks.Runtime (initRuntime)
import           Network.MoeSocks.Type (Env, Job(..), MoeMonad, env, jobs)
import qualified Network.MoeSocks.Type.Bootstrap.Option as O
import           Prelude hiding ((-), take)


runJob :: Env -> Job -> IO ()
runJob aEnv (RemoteRelayJob x)  = runRemoteRelay aEnv x
runJob aEnv (LocalServiceJob x)  = runLocalService aEnv x

runApp :: Env -> [Job] -> IO ()
runApp _ [] = error_ "No job to run"
runApp aEnv someJobs = do
  _asyncs <- mapM (async . foreverRun . runJob aEnv) someJobs
  waitAnyCancel - _asyncs

  pure ()


moeApp:: O.Options -> MoeMonad ()
moeApp someOptions = do
  let _options = someOptions
  _config <- loadConfig - _options

  _runtime <- initRuntime _config _options

  io - runApp (_runtime ^. env) (_runtime ^. jobs)
