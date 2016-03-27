# Handling exception in persistence

Say, you have a code like this which throws exception:

``` haskell
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    age Int
    UniqueAge age
    deriving Show
|]

getUser :: MonadIO m => ReaderT SqlBackend m [Entity User]
getUser = selectList [] []

insertJane :: MonadIO m => ReaderT SqlBackend m ()
insertJane = insert_ $ User 40

sqliteTest :: IO ()
sqliteTest = runSqlite ":memory:" $ do
               runMigration migrateAll

               insertJane
               insertJane

               users <- getUser
               liftIO $ print (users :: [Entity User])
```

The above code will throw exception when executed because the uniqueness constraint is being violated:

```
$ ./sqlite-code

Migrating: CREATE TABLE "user"("id" INTEGER PRIMARY KEY,"age" INTEGER NOT NULL,CONSTRAINT "unique_age" UNIQUE ("age"))
persistent-try-bugs: SQLite3 returned ErrorConstraint while attempting to perform step.
```

You can use the [exceptions](https://hackage.haskell.org/package/exceptions) package to handle exceptions. Have the appropriate imports:

>>> import Control.Exception (SomeException)
>>> import Control.Monad.Catch

And the rest of the code is like this:

``` haskell
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    age Int
    UniqueAge age
    deriving Show
|]

getUser :: MonadIO m => ReaderT SqlBackend m [Entity User]
getUser = selectList [] []

insertJane :: (MonadCatch m, MonadIO m) => ReaderT SqlBackend m ()
insertJane = (insert_ (User 40)) `catch` (\(SomeException e) -> return ())

sqliteTest :: IO ()
sqliteTest = runSqlite ":memory:" $ do
               runMigration migrateAll

               insertJane
               insertJane

               users <- getUser
               liftIO $ print (users :: [Entity User])
```

And this time, it will execute without crashing:

``` 
$ ./sqlite-code

Migrating: CREATE TABLE "user"("id" INTEGER PRIMARY KEY,"age" INTEGER NOT NULL,CONSTRAINT "unique_age" UNIQUE ("age"))
[Entity {entityKey = UserKey {unUserKey = SqlBackendKey {unSqlBackendKey = 1}}, entityVal = User {userAge = 40}}]
```


