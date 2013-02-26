# Run raw SQL

Works with Persistent 0.7, 0.8

```haskell
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, FlexibleContexts, GADTs  #-}
import Data.Conduit (($$), ($=), runResourceT)
import qualified Data.Conduit.List as CL
import Database.Persist
import Database.Persist.Store
import Database.Persist.Sqlite
import Database.Persist.GenericSql
import Database.Persist.GenericSql.Raw
import Control.Monad.IO.Class (liftIO)
import Database.Persist.TH
import Data.Either (rights)
import Data.Text (Text)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Person
    name Text
    deriving Show
|]

main :: IO ()
main = withSqliteConn ":memory:" $ runSqlConn $ do
         runMigration migrateAll
         insert $ Person "Michael Snoyman"
         insert $ Person "Miriam Snoyman"
         insert $ Person "Eliezer Snoyman"
         insert $ Person "Gavriella Snoyman"
         insert $ Person "Greg Weber"
         insert $ Person "Rick Richardson"
         let query = "SELECT name FROM \"Person\";"
         persons <- runResourceT $ withStmt query ([]::[PersistValue])
                    $= CL.map fromPersistValues $$ CL.consume
         liftIO $ print (rights persons::[Person])
```
## In a scaffolded Yesod application

### Running raw queries at app start

```haskell
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    let foundation = App conf s p manager dbconf

    putStrLn "Loading fr collation."
    runNoLoggingT $ do
      -- (Database.Persist.Store.runPool dbconf (runMigration migrateAll) p)
      Database.Persist.Store.runPool dbconf (execute "PRAGMA encoding = 'UTF-8'" []) p
      Database.Persist.Store.runPool dbconf (execute "SELECT icu_load_collation('fr_FR', 'french')" []) p
      return ()
    
    return foundation
```

The code above requires persistent-sqlite v1.1.4.1 so that `execute` accepts queries like `SELECT`.
It also uses `runNoLoggingT` from monad-logger 0.3 (Yesod 1.2).
