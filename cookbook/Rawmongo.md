# Run RAW MongoDB
``` haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           Control.Monad.IO.Class (liftIO)
import           Network (PortID (PortNumber))
import           Database.Persist
import           Database.Persist.MongoDB
import           Database.Persist.TH
import           Language.Haskell.TH.Syntax

let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) {mpsGeneric = False}
 in share [mkPersist mongoSettings] [persistLowerCase|
Person
    name        String
    age         Int         Maybe
    emails      [String]
    address     Address     Maybe
    deriving (Show)
Address
    line1       String
    line2       String      Maybe
    city        String
    state       String
    zip         Int
    deriving (Show)
BlogPost
    title       String
    authorId    PersonId
    deriving (Show)
|]

main :: IO ()
main = withMongoDBConn "test" "localhost" (PortNumber 27017) Nothing 2000 $ \pool -> do
    runMongoDBPool master (do
        let johnAddress = Address "123 W Street Rd." Nothing "Springfield" "Missouri" 65806
        johnId <- insert $ Person "John Doe" (Just 35) ["john@doe.com", "johndoe@mail.com"] (Just johnAddress)
        janeId <- insert $ Person "Jane Doe" Nothing ["jane@doe.com", "janedoe@mail.com"] Nothing

        insert $ BlogPost "My first post!" johnId
        insert $ BlogPost "One more for good measure" johnId

        oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
        liftIO $ print (oneJohnPost :: [Entity BlogPost])

        john <- get johnId
        liftIO $ print (john :: Maybe Person)

        delete janeId
        deleteWhere [BlogPostAuthorId ==. johnId]
        ) pool
```