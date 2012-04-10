    {-
        Below is an example demonstrating how to use slugs in URLs.
    -}
    
    {-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts #-}
    {-# LANGUAGE TemplateHaskell, OverloadedStrings, GADTs, MultiParamTypeClasses #-}

    import Yesod
    import Data.Text
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
        deriving (Show, Read, Eq)

    -- Telling Yesod how to deal with our custom type in paths.   
    instance PathPiece Slug where
        -- Used internally to parse a (Text) path piece.
        -- As the parsing may fail, we return a Maybe value.
        -- fromPathPiece :: Text -> Maybe Slug
        fromPathPiece = return . Slug

        -- Used internally to generate a path piece from a value.
        -- toPathPiece :: Slug -> Text
        toPathPiece = unSlug

    -- Telling Persistent how to store / retrieve a Slug value in the database.        
    instance PersistField Slug where
        -- toPersistValue :: Slug -> PersistValue
        toPersistValue = PersistText . unSlug

        -- fromPersistValue :: PersistValue -> Either Slug
        fromPersistValue v = case v of
                          PersistText text -> Right $ Slug text
                          _ -> Left "Not a PersistText value"
                          
        -- sqlType :: Slug -> SqlType
        sqlType _ = SqlString                

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
    main = withSqlitePool ":memory:" openConnectionCount $ \pool -> do
        runSqlPool (runMigration migrateAll) pool   
        runSqlPool (insert $ Post "My post" "My post <em>content</em>" (Slug "my-post")) pool
        runSqlPool (insert $ Post "Another post" "tl;dr" (Slug "another-post")) pool
        warpDebug 3000 $ YesodBlog pool