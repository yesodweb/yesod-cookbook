# Find nearby locations using Postgres

This snipped is the direct translation of the concept and code presented here in [google developer map resource](https://developers.google.com/maps/articles/phpsqlsearch_v3#findnearsql). The only difference here is that we are using `postgres` database instead of `mysql`.

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

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger    (runStderrLoggingT)
import Control.Monad.Reader

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Markers
    name String 
    address String
    lat Double
    lon Double
    deriving Show
|]

loadData :: MonadIO m => ReaderT SqlBackend m ()
loadData = do
  insert_ $ Markers "Frankie Johnnie & Luigo Too" "939 W El Camino Real, Mountain View, CA" 37.386339 (-122.085823)
  insert_ $ Markers "Amici's East Coast Pizzeria" "790 Castro St  Mountain View  CA" 37.38714 (-122.083235)
  insert_ $ Markers "Kapp's Pizza Bar & Grill" "191 Castro St  Mountain View  CA" 37.393885 (-122.078916)
  insert_ $ Markers "Round Table Pizza: Mountain View" "570 N Shoreline Blvd  Mountain View  CA" 37.402653 (-122.079354)
  insert_ $ Markers "Tony & Alba's Pizza & Pasta" "619 Escuela Ave  Mountain View  CA" 37.394011 (-122.095528)
  insert_ $Markers "Oregano's Wood-Fired Pizza" "4546 El Camino Real  Los Altos  CA" 37.401724 (-122.114646)

connStr = "host=localhost dbname=test1 user=postgres password=postgres port=5432"

data GeoLocation = GeoLocation
    { latitude :: Double,
      longitude :: Double 
    } deriving (Show, Eq, Ord)

findNearbyMarkers :: (MonadIO m) => GeoLocation -> ReaderT SqlBackend m [(Single String, Single Double)]
findNearbyMarkers (GeoLocation lat lon) = rawSql "SELECT name, ( 3959 * acos( cos( radians(37) ) * cos( radians( ? ) ) * cos( radians( ? ) - radians(-122) ) + sin( radians(37) ) * sin( radians( ? ) ) ) ) AS distance FROM markers" [PersistDouble lat, PersistDouble lon, PersistDouble lat]

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connStr 10 $ liftSqlPersistMPool $ do
         runMigration migrateAll
         loadData
         let geo = GeoLocation 37.0 (-122.0)
         markers <- findNearbyMarkers geo
         liftIO $ print "Nearby markers"
         liftIO $ print markers
```

Execution:

``` shellsession
sibi::jane { ~/scripts }-> ./geo.hs
Migrating: CREATe TABLE "markers"("id" SERIAL8  PRIMARY KEY UNIQUE,"name" VARCHAR NOT NULL,"address" VARCHAR NOT NULL,"lat" DOUBLE PRECISION NOT NULL,"lon" DOUBLE PRECISION NOT NULL)
[Debug#SQL] CREATe TABLE "markers"("id" SERIAL8  PRIMARY KEY UNIQUE,"name" VARCHAR NOT NULL,"address" VARCHAR NOT NULL,"lat" DOUBLE PRECISION NOT NULL,"lon" DOUBLE PRECISION NOT NULL); []
[Debug#SQL] INSERT INTO "markers"("name","address","lat","lon") VALUES(?,?,?,?) RETURNING "id"; [PersistText "Frankie Johnnie & Luigo Too",PersistText "939 W El Camino Real, Mountain View, CA",PersistDouble 37.386339,PersistDouble (-122.085823)]
[Debug#SQL] INSERT INTO "markers"("name","address","lat","lon") VALUES(?,?,?,?) RETURNING "id"; [PersistText "Amici's East Coast Pizzeria",PersistText "790 Castro St  Mountain View  CA",PersistDouble 37.38714,PersistDouble (-122.083235)]
[Debug#SQL] INSERT INTO "markers"("name","address","lat","lon") VALUES(?,?,?,?) RETURNING "id"; [PersistText "Kapp's Pizza Bar & Grill",PersistText "191 Castro St  Mountain View  CA",PersistDouble 37.393885,PersistDouble (-122.078916)]
[Debug#SQL] INSERT INTO "markers"("name","address","lat","lon") VALUES(?,?,?,?) RETURNING "id"; [PersistText "Round Table Pizza: Mountain View",PersistText "570 N Shoreline Blvd  Mountain View  CA",PersistDouble 37.402653,PersistDouble (-122.079354)]
[Debug#SQL] INSERT INTO "markers"("name","address","lat","lon") VALUES(?,?,?,?) RETURNING "id"; [PersistText "Tony & Alba's Pizza & Pasta",PersistText "619 Escuela Ave  Mountain View  CA",PersistDouble 37.394011,PersistDouble (-122.095528)]
[Debug#SQL] INSERT INTO "markers"("name","address","lat","lon") VALUES(?,?,?,?) RETURNING "id"; [PersistText "Oregano's Wood-Fired Pizza",PersistText "4546 El Camino Real  Los Altos  CA",PersistDouble 37.401724,PersistDouble (-122.114646)]
[Debug#SQL] SELECT name, ( 3959 * acos( cos( radians(37) ) * cos( radians( ? ) ) * cos( radians( ? ) - radians(-122) ) + sin( radians(37) ) * sin( radians( ? ) ) ) ) AS distance FROM markers; [PersistDouble 37.0,PersistDouble (-122.0),PersistDouble 37.0]
"Nearby cities"
[(Single {unSingle = "Frankie Johnnie & Luigo Too"},Single {unSingle = 0.0}),(Single {unSingle = "Amici's East Coast Pizzeria"},Single {unSingle = 0.0}),(Single {unSingle = "Kapp's Pizza Bar & Grill"},Single {unSingle = 0.0}),(Single {unSingle = "Round Table Pizza: Mountain View"},Single {unSingle = 0.0}),(Single {unSingle = "Tony & Alba's Pizza & Pasta"},Single {unSingle = 0.0}),(Single {unSingle = "Oregano's Wood-Fired Pizza"},Single {unSingle = 0.0})]
```

