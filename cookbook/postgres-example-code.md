Example Posgres Persistent code:

``` haskell
#!/usr/bin/env stack
{- stack
     --resolver lts-7.3
     --install-ghc
     runghc
     --package persistent
     --package persistent-postgresql
     --package persistent-template
     --package network
     --package mtl
-}


{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Peerson
    name String
    age Int
    UniqueAge age
    deriving Show
|]

connStr = "host=localhost dbname=test user=postgres password=postgres port=5432"

main :: IO ()
main =
  runStderrLoggingT $
  withPostgresqlPool connStr 10 $
  \pool ->
     liftIO $
     do flip runSqlPersistMPool pool $
          do runMigration migrateAll
             johnId <- insert $ Peerson "John Doe" 35
             liftIO $ print johnId
             return ()
```
