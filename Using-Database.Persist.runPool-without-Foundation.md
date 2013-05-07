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