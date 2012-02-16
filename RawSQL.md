# Run raw SQL

Works with Persistent 0.7, 0.8

~~~{.haskell}
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
~~~
