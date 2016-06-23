*Updated for the Yesod 1.4.3.3 scaffolding*

In SQLite, foreign keys checks [are not enabled by default](https://www.sqlite.org/foreignkeys.html#fk_enable). Instead, they must be enabled on a per-connection basis by issuing a PRAGMA command, like so:

```sql
PRAGMA foreign_keys = ON;
```

The command to issue foreign keys is [a noop if done inside of a transaction](https://www.sqlite.org/foreignkeys.html#fk_enable), so it is necessary to enable them outside of one. Persistent functions like `runSqlPersistMPool` wrap your statements inside of a transaction, necessitating dropping down to the underlying `Database.Sqlite` package to enable foreign keys. To do this, add the following imports and functions to `Application.hs`:

```haskell
import qualified Database.Sqlite as Sqlite
import Database.Persist.Sqlite (createSqlPool, wrapConnection)

enableForeignKeys :: Sqlite.Connection -> IO ()
enableForeignKeys conn = Sqlite.prepare conn "PRAGMA foreign_keys = ON;" >>= void . Sqlite.step

createSqliteBackend :: Text -> LogFunc -> IO SqlBackend
createSqliteBackend connStr logFunc = do
  conn <- Sqlite.open connStr
  enableForeignKeys conn
  wrapConnection conn logFunc

```

Then, in the `makeFoundation` function, replace this code:
```
-- Create the database connection pool
pool <- flip runLoggingT logFunc $ createSqlitePool
    (sqlDatabase $ appDatabaseConf appSettings)
    (sqlPoolSize $ appDatabaseConf appSettings)
```

with this:

```
let dbPath = sqlDatabase (appDatabaseConf appSettings)
pool <- flip runLoggingT logFunc $ createSqlPool
        (createSqliteBackend dbPath)
        (sqlPoolSize $ appDatabaseConf appSettings)
```

You can then verify that foreign keys are enabled by sending `PRAGMA foreign_keys` to SQLite:

```haskell
import Database.Persist.Sql (SqlBackend, rawSql, unSingle)

fksEnabled :: MonadIO m => ReaderT SqlBackend m Bool
fksEnabled = do
    fkStatus <- rawSql "PRAGMA foreign_keys" []
    return $ (map unSingle fkStatus) == ["1" :: Text]
```

You can then call this function from a Handler or a test, like so:

```haskell
fkStatus <- runDB $ fksEnabled
traceM $ "Foreign keys enabled = " ++ show fkStatus
```
