Custom data types column in Persistent
===============================================

*NOTE: This article just demonstrates how to use this technique. But
in actual practice you should probably store each value in
MyCustomTuple as an individual column. This is what Michael Snoyman
had to say on this: "It's bad from a DB management standpoint: it's
just storing the data as JSON in the table, which is difficult to work
with. The proper where is to just have two different fields in the
entity. Whether you want to do it one way or the other though is up to
personal preference, both work." See
[here](https://github.com/yesodweb/yesod-cookbook/pull/11#issuecomment-247297565)
for more information.*

Let's say you have a custom datatype like

``` haskell
data MyCustomTuple = MyCustomTuple (Int, Int) deriving Show
```

and you want to have it as Column in `persistent` schema:

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String 
    customField MyCustomTuple
    deriving Show
|]


Then you are likely to an error in the form of:

``` haskell
    No instance for (PersistFieldSql MyCustomTuple)
      arising from a use of ‘sqlType’
    In the fourth argument of ‘FieldDef’, namely
      ‘sqlType (Data.Proxy.Proxy :: Data.Proxy.Proxy MyCustomTuple)’
    In the expression:
      FieldDef
        (HaskellName (packPTH "test2"))
        (DBName (packPTH "test2"))
        (FTTypeCon Nothing (packPTH "MyCustomTuple"))
        (sqlType (Data.Proxy.Proxy :: Data.Proxy.Proxy MyCustomTuple))
        []
        True
        NoReference
    In the ‘entityFields’ field of a record
```

To make it work, you have create proper instances of your type with
the typeclasses `PersistField` and `PersistFieldSql`. A sample example working code:

``` haskell
#!/usr/bin/env stack
{- stack
     --resolver lts-6.11
     --install-ghc
     runghc
     --package persistent
     --package persistent-postgresql
     --package persistent-template
     --package mtl
     --package time
     --package text
-}

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Data.Time
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import           Control.Monad.Reader
import           Restriction  -- because of GHC stage restriction

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String 
    test MyCustomTuple
    deriving Show
|]

connStr = "host=localhost dbname=test user=postgres password=postgres port=5432"

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connStr 10 $ liftSqlPersistMPool $ do
         runMigration migrateAll
         insert_ $ Person "Sibi" (MyCustomTuple (3,8))
         (xs :: [Entity Person]) <- selectList [] []
         liftIO $ print xs
```

And the other module:

``` haskell
{-#LANGUAGE OverloadedStrings#-}

module Restriction where

import           Database.Persist
import           Database.Persist.Sql
import qualified Data.Text as T
import Data.Monoid ((<>))

data MyCustomTuple = MyCustomTuple (Int, Int) deriving Show

instance PersistField MyCustomTuple where
    toPersistValue (MyCustomTuple (a,b)) = PersistList [PersistInt64 (fromIntegral a), PersistInt64 (fromIntegral b)]
    fromPersistValue (PersistText t) = case read $ T.unpack t of
                                         [a,b] -> Right $ MyCustomTuple (a,b)
                                         _ -> Left $ "Expected String representing list with two values but received: " <> t
    fromPersistValue x = Left $ T.pack $ "Expected PersistList with two values but received: " ++ show x

instance PersistFieldSql MyCustomTuple where
    sqlType _ = SqlString
```

On Execution
--------------

``` shellsession
sibi::jane { ~/scripts }-> ./acustom.hs
Migrating: CREATe TABLE "person"("id" SERIAL8  PRIMARY KEY UNIQUE,"name" VARCHAR NOT NULL,"test" VARCHAR NOT NULL)
[Debug#SQL] CREATe TABLE "person"("id" SERIAL8  PRIMARY KEY UNIQUE,"name" VARCHAR NOT NULL,"test" VARCHAR NOT NULL); []
[Debug#SQL] INSERT INTO "person"("name","test") VALUES(?,?) RETURNING "id"; [PersistText "Sibi",PersistList [PersistInt64 3,PersistInt64 8]]
[Debug#SQL] SELECT "id", "name", "test" FROM "person"; []
[Entity {entityKey = PersonKey {unPersonKey = SqlBackendKey {unSqlBackendKey = 1}}, entityVal = Person {personName = "Sibi", personTest = MyCustomTuple (3,8)}}]
```

