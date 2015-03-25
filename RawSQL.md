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

### Fetching data

Combine the [rawSql](http://hackage.haskell.org/packages/archive/persistent/1.1.5.1/doc/html/Database-Persist-GenericSql.html#v:rawSql) function with `runDB` (which is defined in "Foundation.hs").

```haskell
getSearchR :: Text -> Handler RepHtml
getSearchR pattern = do
    users <- selectUsers pattern
    defaultLayout $ do
        -- [...]
  where
    selectUsers :: Text -> Handler [Entity User]
    selectUsers t = runDB $ rawSql s [toPersistValue t]
      where s = "SELECT ?? FROM user WHERE name = ? ORDER BY (role_id IS NULL) ASC, name DESC"
```
The output of `rawSql` is constrained by the type expected, here `[Entity User]`.
The "??" within the query is filled according to this expected type.

If the type was `[(Entity User, Entity Role)]` because of a join, the query would begin with `SELECT ??, ??`.
In case the select returns a list of fields like in `SELECT id, lowercase(name)`, then the type should look like `[(Single UserId, Single PersistValue)]`.

Returned data can be handled through [entityKey](http://hackage.haskell.org/packages/archive/persistent/1.1.5.1/doc/html/Database-Persist-GenericSql.html#v:Entity), [entityVal](http://hackage.haskell.org/packages/archive/persistent/1.1.5.1/doc/html/Database-Persist-GenericSql.html#v:Entity) or [fromPersistValue](http://hackage.haskell.org/packages/archive/persistent/1.1.5.1/doc/html/Database-Persist-Store.html#v:fromPersistValue).

### Running raw queries at app start

Within Application.hs:

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

Unfortunately, SQLite's pragmas and collations are not persistent: they have to be declared for each connection.
Persistent-sqlite makes this possible since its version 1.5.0.
So the code above won't work except for the first Sqlite connection.
With a pool of connections, the code above should be replaced by:

```haskell
import Database.Persist.Sqlite
import Database.Persist.GenericSql.Internal (createSqlPool)
import qualified Database.Sqlite as Sqlite

-- [...]

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- createSqlitePoolConn (dbconf :: Settings.PersistConfig)
    return $ App conf s p manager dbconf

createSqlitePoolConn :: SqliteConf -> IO ConnectionPool
createSqlitePoolConn (SqliteConf cs size) = do
    newConn <- customConn cs
    createSqlPool (wrapConnection newConn) size
  where
      customConn :: Text -> IO Connection
      customConn s = Sqlite.open s >>= execInit >>= wrapConnection

      execInit :: Sqlite.Connection -> IO Sqlite.Connection
      execInit conn = execute conn "SELECT icu_load_collation('fr_FR', 'french')"

      execute :: Sqlite.Connection -> Text -> IO ()
      execute conn sql = do
        stmt <- Sqlite.prepare conn sql
        _ <- Sqlite.step stmt
        Sqlite.finalize stmt
        return ()
```