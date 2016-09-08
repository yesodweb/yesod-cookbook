You can use persistent to model your DB schema and see the generated
sql query without even the presence of the actual database
server. Example code demonstrating it:

``` haskell
#!/usr/bin/env stack
{- stack
     --resolver lts-6.11
     --install-ghc
     runghc
     --package persistent
     --package persistent-postgresql
     --package persistent-template
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

import           Database.Persist
import           Database.Persist.Postgresql
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

main :: IO ()
main = mockMigration migrateAll
```

On execution, this will produce:

``` shellsession
sibi { ~/scripts }-> ./migration.hs
CREATe TABLE "person"("id" SERIAL8  PRIMARY KEY UNIQUE,"name" VARCHAR NOT NULL,"age" INT8 NULL,"address" INT8 NOT NULL)
CREATe TABLE "blog_post"("id" SERIAL8  PRIMARY KEY UNIQUE,"title" VARCHAR NOT NULL,"author_id" INT8 NOT NULL)
ALTER TABLE "blog_post" ADD CONSTRAINT "blog_post_author_id_fkey" FOREIGN KEY("author_id") REFERENCES "person"("id")
```

