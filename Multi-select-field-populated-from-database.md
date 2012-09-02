Example of a product list where each product optionally has multiple categories (through a many-to-many relationship, using a linker table.

This example shows how to fill a multi select box with values from the Category table, and use the selected values to create a new Product.

    {-# LANGUAGE TypeFamilies, TemplateHaskell, MultiParamTypeClasses,
    GADTs, QuasiQuotes, OverloadedStrings, FlexibleContexts #-}
    import Yesod
    import Database.Persist
    import Database.Persist.Sqlite
    import Control.Applicative (pure, (<$>), (<*>))
    import Data.Text (Text, concat)
    import Data.List (intersperse)
    
    share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
    Category
        name Text
        deriving Show
    Product
        name Text
        deriving Show
    ProductCategory
        product ProductId
        category CategoryId
        UniqueProductCategory product category
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
                        Categories
                $forall row <- rows
                    <tr>
                        <td>
                            #{productName $ fst row}
                        <td>
                            #{Data.Text.concat $ intersperse ", " (Prelude.map categoryName (snd row))}
    |]
    
    productsAndCategories :: GHandler App App [(Product, [Category])]
    productsAndCategories = runDB $ selectList [] [Asc ProductName] >>= mapM (\(Entity kp p) -> do
        categoryProducts <- selectList [ProductCategoryProduct ==. kp] []
        let categoryIds = Prelude.map (productCategoryCategory . entityVal) categoryProducts
        categoryEntities <- selectList [CategoryId <-. categoryIds] []
        return (p, Prelude.map entityVal categoryEntities))                                       
                                           
    postHomeR :: Handler RepHtml
    postHomeR = do
        ((result, _), _) <- runFormPostNoToken $ productForm Nothing
        case result of 
            FormSuccess (name, maybeCategories) -> do
                p <- runDB $ insert $ Product name
                case maybeCategories of
                    Just c -> mapM_ (\c' -> runDB $ insert $ ProductCategory p c') c 
                    Nothing -> return ()
                redirect HomeR
            _ -> do
                setMessage "Failure adding"
                redirect HomeR
                       
    productForm :: Maybe Product -> Html -> MForm App App (FormResult (Text, Maybe [CategoryId]), Widget)
    productForm mproduct = renderBootstrap $ (,)
        <$> areq textField "Name" (productName <$> mproduct)
        <*> aopt (multiSelectField categories) "Categories" Nothing
        where
            categories = do
                entities <- runDB $ selectList [CategoryName !=. ""] [Asc CategoryName]
                optionsPairs $ Prelude.map (\cat -> (categoryName $ entityVal cat, entityKey cat)) entities
            categories :: GHandler App App (OptionList CategoryId)
    
    openConnectionCount :: Int
    openConnectionCount = 10
    
    main :: IO ()
    main = withSqlitePool ":memory:" openConnectionCount $ \pool -> do
        flip runSqlPool pool $ do
            runMigration migrateAll
            
            -- add some example data
            
            -- categories
            home <- insert $ Category "Home, Garden & Tools"
            kitchen <- insert $ Category "Kitchen & Dining"
            toys <- insert $ Category "Toys & Games"
            clothing <- insert $ Category "Clothing"
    
            -- products
            chair <- insert $ Product "Vinyl chair"
            insert $ ProductCategory chair home
            
            coffeemaker <- insert $ Product "Coffeemaker"
            insert $ ProductCategory coffeemaker kitchen
            -- and the second category:
            insert $ ProductCategory coffeemaker home
            
            nerf <- insert $ Product "Nerf Blaster"
            insert $ ProductCategory nerf toys
            
            dress <- insert $ Product "Urban Sprawl Print Hi-low Dress"
            insert $ ProductCategory dress clothing
            
            insert $ Product "Milkshake"
            -- no category
            
            return ()
            
        warpDebug 3000 $ App pool