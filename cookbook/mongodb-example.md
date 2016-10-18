# MongoDB Sample code with persistent

``` haskell
#!/usr/bin/env stack
{- stack
     --resolver lts-6.19
     --install-ghc
     runghc
     --package persistent
     --package persistent-mongoDB
     --package persistent-template
     --package network
     --package mtl
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

import Database.Persist
import Database.Persist.TH
import Database.Persist.MongoDB
import Control.Monad.Cont
import MongoImport
import Network (PortID(PortNumber))
import Language.Haskell.TH (Type(..))

share
    [mkPersist mongoSettings]
    [persistLowerCase|
User
    name String
    age Int Maybe
    deriving Show
Blogpost
    title String
    uid UserId
    UniqueUser uid
    deriving Show
|]

main :: IO ()
main =
    withMongoDBConn
        "myDatabaseName"
        "localhost"
        (PortNumber 27017)
        Nothing
        2000
        (runMongoDBPool
             master
             (do user <- insert $ User "John Doe" $ Just 35
                 liftIO $ print user
                 return ()))
```

You also have to have the `MongoImport.hs` modul along with it:

``` haskell
{-# LANGUAGE TemplateHaskell #-}

module MongoImport where

import Language.Haskell.TH (Type(..))
import Database.Persist.TH
import Database.Persist.MongoDB

mongoSettings =
    (mkPersistSettings (ConT ''MongoContext))
    { mpsGeneric = False
    }
```

Execution demo:

``` shellsession
sibi::jane { ~/scripts }-> ./mongodb.hs
UserKey {unUserKey = MongoKey {unMongoKey = 5801bde35844a10b1f000000}}
```

