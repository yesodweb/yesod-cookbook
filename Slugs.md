```haskell
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, GADTs, MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

import Yesod
import Yesod.Form.Jquery
import Text.Blaze (preEscapedToMarkup)
import Data.Text
import Database.Persist
import Database.Persist.Store
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.Error
import Control.Applicative ((<$>), (<*>))

-- Our foundation type.
data YesodBlog = YesodBlog {dbPool :: ConnectionPool}

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
/admin/posts/new NewPostR GET POST
|]

instance Yesod YesodBlog
instance RenderMessage YesodBlog FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist YesodBlog where
    type YesodPersistBackend YesodBlog = SqlPersist
    runDB action = do
        YesodBlog pool <- getYesod
        runSqlPool action pool

isEmpty :: Text -> Bool
isEmpty = const False

slugField :: (RenderMessage master FormMessage) => Field sub master Slug
slugField = Field
            { fieldParse = \rawVals -> case rawVals of
                                   [s] 
                                       | isEmpty s -> return $ Left "Empty slug forbidden"
                                       | otherwise -> return $ Right $ Just $ Slug s
                                   _ -> return $ Left "A value must be provided"
            , fieldView = \idAttr nameAttr attrs eResult isReq ->
                          (fieldView textField) idAttr nameAttr attrs (eResult >>= return . unSlug) isReq
            }

postForm :: Html -> MForm YesodBlog YesodBlog (FormResult Post, Widget)
postForm = renderDivs $ Post
           <$> areq textField "Title" Nothing
           <*> areq textField "Content" Nothing
           <*> areq slugField "Slug" Nothing
      

getNewPostR :: Handler RepHtml
getNewPostR = do
  (w, enctype) <- generateFormPost postForm
  defaultLayout [whamlet|
<h1>Create Post
<form method=post action=@{NewPostR} enctype=#{enctype}>
      ^{w}
      <input type="submit">
|]

postNewPostR :: Handler RepHtml
postNewPostR = do
  ((result, widget), enctype) <- runFormPost postForm
  case result of
    FormSuccess post@Post {postSlug, ..} -> do
                 pool <- fmap dbPool getYesod
                 Yesod.lift $ runSqlPool (insertBy post) pool
                 defaultLayout [whamlet|
<p>Post added !
<a href=@{PostR postSlug}>Click here to read.
|]
    _ -> defaultLayout [whamlet|
<p>Failed to add post.
<form method=post action=@{NewPostR} enctype=#{enctype}>
    ^{widget}
    <input type=submit>
|]

getRootR :: Handler RepHtml
getRootR = do
         posts <- runDB $ selectList [] [Asc PostTitle]
         defaultLayout [whamlet|
<h1>My blog posts
<ul #posts>
    $forall Entity id post <- posts
            <li>
                <a href=@{PostR $ postSlug post}>#{postTitle post}
<hr>
<a href=@{NewPostR}>Click here to add posts
|]

getPostR :: Slug -> Handler RepHtml
getPostR slug = do
         Entity _ Post {postTitle, postContent, ..} <- runDB $ getBy404 $ UniqueSlug slug
         defaultLayout [whamlet|
<h1>#{postTitle}
<div #post-content>
     #{preEscapedToMarkup postContent}
<hr>
<a href=@{RootR}>List of posts
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