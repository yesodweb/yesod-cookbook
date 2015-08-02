Example MySQL connection code:

```haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.MySQL
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    address Int
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

connectionInfo = defaultConnectInfo { connectPort = 5000,
                                      connectPassword = "password",
                                      connectDatabase = "database"}

main :: IO ()
main = runStderrLoggingT $ withMySQLPool connectionInfo 10 $ \pool -> liftIO $ do
         flip runSqlPersistMPool pool $ do
           printMigration migrateAll
```