# Adding Seed data to a Scaffolded Site
This describes how to create a small executable to add data to a database in a scaffolded Yesod application. The description is based on persistent >= 2.0 and PostgreSQL.

The approach is to define an executable named `Seed` in cabal and the data in a Haskell file at `test/Seed.hs`. The database can then be seeded by running `$ cabal run Seed`.

```
-- ExampleApp.cabal
...
executable         Seed
    if flag(library-only)
        Buildable: False

    main-is:           Seed.hs
    hs-source-dirs:    test
    build-depends:     base
                     , ExampleApp
                     , monad-logger
                     , persistent
                     , persistent-postgresql
```

```
-- test/Seed.hs
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
import Import
import Model
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql (pgConnStr, withPostgresqlConn, runSqlConn, rawExecute)

insertUser :: MonadIO m => (Text, Maybe Text) -> ReaderT SqlBackend m ()
insertUser (ident, passwd) =
  insert_ $ User ident passwd 

users :: [(Text, Maybe Text)]
users =
  [ ("bob",     Just "s3cr3t")
  , ("charles", Just "passw0rd")
  ]

main :: IO ()
main = do
  settings <- loadAppSettingsArgs [configSettingsYmlValue] useEnv
  let conn = (pgConnStr $ appDatabaseConf settings)
  runStderrLoggingT . withPostgresqlConn conn $ runSqlConn $ do
    runMigration migrateAll
    mapM_ insertUser users
```