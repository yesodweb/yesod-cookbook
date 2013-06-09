Here's a simple setup for initializing Persistent connection pool using scaffolded database settings and running queries but without Foundation-setup. 

```haskell

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Import
import Yesod.Default.Config
import qualified Database.Persist
import Settings
import Model
import Data.Conduit (runResourceT)
import Control.Monad.Logger (runStdoutLoggingT)

runQueries = do
    users <- selectList [UserIdent ==. "test"] []
    return ()

main :: IO ()
main = do
    conf <- Yesod.Default.Config.loadConfig $ (configSettings Production)

    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)

    runStdoutLoggingT $ runResourceT $ Database.Persist.runPool dbconf runQueries p
```


Using outside of a Yesod app, showing the creationg of an environment for runDB.
This was abstracted from working code, it should be close to compiling.

``` haskell
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl (‥))

import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Logger -- (runStdoutLoggingT, LoggingT)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)

import Data.Yaml (decodeFile, parseMonad)
import Data.Aeson (Value)
import qualified Data.Map as M


default (Text)

data WorkerConf = WorkerConf {
                    getConfig ∷ MongoConf -- MongoDB, use your database config type
                  , getPool :: PersistConfigPool MongoConf
                  }   

type WorkerM = ReaderT WorkerConf (ResourceT (LoggingT IO))

type ControlIO m = (MonadIO m, MonadBaseControl IO m)

makeRunDB ∷ ControlIO m ⇒ WorkerConf → Action m a → m a 
makeRunDB workerConf f = Database.Persist.runPool (getConfig workerConf) f (getPool workerConf)

runDB ∷ Action WorkerM a → WorkerM a
runDB f = do
  workerConf ← ask
  makeRunDB workerConf f

main ∷ IO ()
main = do 
    dbConfJson ← justEnv "Development" `fmap` loadYaml "mongoDB.yaml"
    dbConf ← parseMonad Database.Persist.loadConfig dbConfJson
    pool ← Database.Persist.createPoolConfig dbConf 
    let workerConf = WorkerConf dbConf pool

    runStdoutLoggingT $ runResourceT $ flip runReaderT workerConf $ do
    res <- runDB ...
    return ()

justEnv ∷ Text → M.Map Text Value → Value
justEnv envName obj =
    case M.lookup envName obj of
      Nothing → error "could not find environment"
      Just env → env

loadYaml ∷ String → IO (M.Map Text Value)
loadYaml fp = do
    mval ← decodeFile fp
    case mval of
      Nothing  → error $ "Invalid YAML file: " ⧺ show fp
      Just obj → return obj

```