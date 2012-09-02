    {-# LANGUAGE TypeFamilies, TemplateHaskell, MultiParamTypeClasses,
    GADTs, QuasiQuotes, OverloadedStrings, FlexibleContexts #-}
    import Yesod
    import Database.Persist
    import Database.Persist.Sqlite
    import Control.Applicative ((<$>), (<*>))
    import Data.Text
    
    share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
    Category
        name Text
        deriving Show
    Product
        name Text
        category CategoryId Maybe
        deriving Show
    |]
    
    data App = App ConnectionPool
    
    mkYesod "App" [parseRoutes|
    / HomeR GET POST
    |]
    
    instance Yesod App
    instance YesodPersist App where
        type YesodPersistBackend App = SqlPersist
        runDB action = do
            App pool <- getYesod
            runSqlPool action pool
    
    instance RenderMessage App FormMessage where
        renderMessage _ _ = defaultFormMessage
    
    -- load Twitter Bootstrap styles
    addStyle :: Widget
    addStyle = addStylesheetRemote "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.1.0/css/bootstrap-combined.min.css"
    
    getHomeR :: Handler RepHtml
    getHomeR = do
        rows <- productsAndCategories
        ((result, formWidget), enctype) <- runFormGet $ productForm Nothing
    
        defaultLayout $ do
            addStyle
            [whamlet|$newline never
    <div .container>
        <div .row>
            <h2>
                Add new product
            <form method=post enctype=#{enctype}>
                ^{formWidget}
                <input type=submit .btn .btn-primary value="Save">
            <h2>
                Products
            <table .table>
                <tr>
                    <th>
                        Product name
                    <th>
                        Category
                $forall row <- rows
                    <tr>
                        <td>
                            #{productName $ fst row}
                        <td>
                            $maybe category <- snd row
                                #{categoryName $ category}
    |]
    
    productsAndCategories :: GHandler App App [(Product, Maybe Category)]
    productsAndCategories = runDB $ selectList [] [Asc ProductName] >>= mapM (\(Entity _ p) -> do
        category <- case (productCategory p) of
            Just c -> do
                get c
            Nothing -> return Nothing
        return (p, category))                                       
    
    postHomeR :: Handler RepHtml
    postHomeR = do
        ((result, _), _) <- runFormPostNoToken $ productForm Nothing
        case result of 
            FormSuccess product -> do
                _ <- runDB $ insert product
                redirect HomeR
            _ -> do
                setMessage "Failure adding"
                redirect HomeR
    
    productForm :: Maybe Product -> Html -> MForm App App (FormResult Product, Widget)
    productForm mproduct = renderBootstrap $ Product
        <$> areq textField "Name" (productName <$> mproduct)
        <*> aopt (selectField categories) "Category" (productCategory <$> mproduct)
        where
            categories :: GHandler App App (OptionList CategoryId)
            categories = do
                entities <- runDB $ selectList [] [Asc CategoryName]
                optionsPairs $ Prelude.map (\cat -> (categoryName $ entityVal cat, entityKey cat)) entities
    
    openConnectionCount :: Int
    openConnectionCount = 10
    
    main :: IO ()
    main = withSqlitePool ":memory:" openConnectionCount $ \pool -> do
        flip runSqlPool pool $ do
            runMigration migrateAll
    
            -- add some example data
            home <- insert $ Category "Home, Garden & Tools"
            insert $ Product "Vinyl chair" (Just home)
    
            kitchen <- insert $ Category "Kitchen & Dining"
            insert $ Product "Coffeemaker" (Just kitchen)
    
            toys <- insert $ Category "Toys & Games"
            insert $ Product "Nerf Blaster" (Just toys)
    
            clothing <- insert $ Category "Clothing"
            insert $ Product "Urban Sprawl Print Hi-low Dress" (Just clothing)
    
            insert $ Product "Milkshake" Nothing
    
            return ()
    
        warpDebug 3000 $ App pool