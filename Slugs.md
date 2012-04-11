```haskell
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, GADTs, MultiParamTypeClasses #-}

import Yesod
import Data.Text
import Text.Blaze (preEscapedText)
import Database.Persist
import Database.Persist.Store
import Database.Persist.Sqlite
import Database.Persist.TH

-- Our foundation type.
data YesodBlog = YesodBlog ConnectionPool

-- Using a new type for Slug. Simply a wrapper around a text value.
newtype Slug = Slug {unSlug :: Text}
        deriving (Show, Read, Eq, PathPiece, PersistField)
--         
-- We have to derive PathPiece and PersistField so that our type
-- can be used in routes and stored with Persistent.
--
-- Here, we use automatic deriving and let the compiler figure out the details for us.
-- To quote Michael Snoyman: "It's a bit terser and safer to just use automatic 
-- deriving of the PathPiece and PersistField instances. Case in point: some backends
-- will return the text data as UTF-8 encoded bytestrings, which the
-- implementation of fromPersistValue won't pick up."
--
-- For pedagogical purposes, though, here is how I first implemented them.
-- 
-- -- Telling Yesod how to deal with our custom type in paths.   
-- instance PathPiece Slug where
--         -- Used internally to parse a (Text) path piece.
--         -- As the parsing may fail, we return a Maybe value.
--         -- fromPathPiece :: Text -> Maybe Slug
--         fromPathPiece = return . Slug

--         -- Used internally to generate a path piece from a value.
--         -- toPathPiece :: Slug -> Text
--         toPathPiece = unSlug

-- -- Telling Persistent how to store / retrieve a Slug value in the database.        
-- instance PersistField Slug where
--          -- toPersistValue :: Slug -> PersistValue
--          toPersistValue = PersistText . unSlug

--          -- fromPersistValue :: PersistValue -> Either Slug
--          fromPersistValue v = case v of
--                           PersistText text -> Right $ Slug text
--                           _ -> Left "Not a PersistText value"
                          
--          -- sqlType :: Slug -> SqlType
--          sqlType _ = SqlString                

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Post
        title Text
        content Text
        slug Slug
        UniqueSlug slug
        deriving Show
|]     

mkYesod "YesodBlog" [parseRoutes|
/ RootR GET
/post/#Slug PostR GET
|]

instance Yesod YesodBlog
instance YesodPersist YesodBlog where
    type YesodPersistBackend YesodBlog = SqlPersist
    runDB action = do
        YesodBlog pool <- getYesod
        runSqlPool action pool


getRootR :: Handler RepHtml
getRootR = do
         posts <- runDB $ selectList [] [Asc PostTitle]
         defaultLayout [whamlet|
<h1>My blog posts
<ul #posts>
    $forall Entity id post <- posts
            <li>
                <a href=@{PostR $ postSlug post}>#{postTitle post}
|]

getPostR :: Slug -> Handler RepHtml
getPostR slug = do
         Entity key post <- runDB $ getBy404 $ UniqueSlug slug
         defaultLayout [whamlet|
<h1>#{postTitle post}
<div #post-content>
     #{preEscapedText $ postContent post}
|]

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = withSqlitePool "slug.db3" openConnectionCount $ \pool -> do
     runSqlPool (runMigration migrateAll) pool
     -- Inserting some posts...
     runSqlPool (insertBy $ Post "My post" "My post <em>content</em>" (Slug "my-post")) pool
     runSqlPool (insertBy $ Post "Another post" "tl;dr" (Slug "another-post")) pool
     warpDebug 3000 $ YesodBlog pool
```           