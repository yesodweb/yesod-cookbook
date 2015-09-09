There may be scenarios in which you may want to connect to an
additional existing database apart from your own application database.

One way as suggested by Hugo Daniel in this [thread](https://groups.google.com/d/msg/yesodweb/c2AwOzaeAD0/oAeIN-eutF8J) is to create multiple instances of YesodPerist. This involves changing your
Application.hs, Foundation.hs and adding other files to make it work.
Although, this approach works I think it involves lot of work and I
wouldn't want to disturb the scaffolded site template.

Another alternative as described here is to create a standalone module
for interacting with the existing database instead. For this example,
I would assume that you want to connect to an existing sqlite
database. The same method can be applied for other database backends.

So, initially using sqlite3 command line program, I see the schema of
the database.

**$ sqlite3 test.db**

**sqlite> .schema**

**CREATE TABLE test_sync (CHANGE NUMERIC, START NUMERIC, PAUSE NUMERIC, RESUME NUMERIC, STOP NUMERIC, sync_id INTEGER PRIMARY KEY);**

So I have a table named test_sync with various field in it. sync_id is
the primary key in the table. Now all you have to create is a separate
module for accessing this table in a typesafe manner:

```haskell
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.TestDB where

import Database.Persist.Sqlite
import Prelude
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.TH

share [mkPersist sqlSettings] [persistLowerCase|
TestSync sql=test_sync id=sync_id
    change Int sql=CHANGE
    start Int sql=START
    pause Int sql=PAUSE
    resume Int sql=RESUME
    stop Int sql=STOP
    deriving Show

|]

pravasConf = SqliteConf {
  sqlDatabase = "/home/sibi/test.db"
  sqlPoolSize = 2
}

runSimDB f = runSqlite (sqlDatabase pravasConf) $ do
  f
```

You can run all your database transactions inside the function
runSimDB. An example of using this function in an handler:

```haskell
getLayerR :: Handler Html
getLayerR = someLayout $ do
  setTitle "GIS Layers"
  runSimDB $ do
     sample <- selectFirst [TestSyncStart >=. 0] []
     liftIO (print sample) 
```

Now wasn't that simple ? :-)     