This is an enhanced version of "Multi select field populated from database".

***

Example of a product list where each product optionally has multiple categories (through a many-to-many relationship, using a linker table.

This example shows how to fill a multi select box with values from the Category table, and use the selected values to create a new Product.


    {-# LANGUAGE FlexibleContexts
               , GADTs
               , MultiParamTypeClasses
               , OverloadedStrings
               , QuasiQuotes
               , TemplateHaskell
               , TypeFamilies
     #-}
    
    import Yesod hiding ((==.), (!=.))
    import Database.Esqueleto
    import Database.Persist.Sqlite (withSqliteConn, withSqlitePool)
    import Control.Applicative (pure, (<$>), (<*>))
    import Data.Text (Text, concat)
    import Control.Monad (forM_, liftM)
    import qualified Data.Map.Strict as Map
    import Data.List (foldl', intersperse, sort)
    import Data.Maybe (catMaybes)
    
    share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
    Product
        name Text
        deriving Eq Ord Show
    Category
        name Text
        deriving Eq Ord Show
    ProductCategory
        product ProductId
        category CategoryId
        deriving Eq Ord Show
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
    
    getProductsAndCategoriesFromDatabase :: GHandler App App [(Entity Product, Maybe (Entity Category))]
    getProductsAndCategoriesFromDatabase = runDB $ select $
        from    $ \(p `LeftOuterJoin` mpc `LeftOuterJoin` mc) -> do
        on      $      mc ?. CategoryId ==. mpc ?. ProductCategoryCategory
        on      $ just (p ^. ProductId) ==. mpc ?. ProductCategoryProduct
        orderBy [ asc (p ^. ProductName) ]
        return (p, mc)
    
    keyValuesToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]  
    keyValuesToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs
    
    productsAndCategoriesToValues :: [(Entity Product, [Maybe (Entity Category)])] -> [(Product, [Category])]
    productsAndCategoriesToValues = map toValues
        where
            toValues (p, cs) = (entityVal p, map entityVal . catMaybes $ cs)
     
    getHomeR :: Handler RepHtml
    getHomeR = do
        pcs <- getProductsAndCategoriesFromDatabase
        let rows = sort . productsAndCategoriesToValues . Map.toList . keyValuesToMap $ pcs
    
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
                $forall (eProduct, eCategories) <- rows
                    <tr>
                        <td>
                            #{productName eProduct}
                        <td>
                            #{Data.Text.concat $ intersperse ", " (Prelude.map categoryName $ eCategories)}
    |]
                           
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
                entities <- runDB $ select $
                                    from $ \cat -> do
                                    orderBy [asc (cat ^. CategoryName)]
                                    return cat
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
            apparel <- insert $ Category "Apparel"
            utensils <- insert $ Category "Utensils"
    
            -- products
            chair <- insert $ Product "Vinyl chair"
            insert $ ProductCategory chair home
            
            coffeemaker <- insert $ Product "Coffeemaker"
            insert $ ProductCategory coffeemaker kitchen
            -- and the second category:
            insert $ ProductCategory coffeemaker home
            -- and the third category:
            insert $ ProductCategory coffeemaker utensils
            
            nerf <- insert $ Product "Nerf Blaster"
            insert $ ProductCategory nerf toys
            
            dress <- insert $ Product "Urban Sprawl Print Hi-low Dress"
            insert $ ProductCategory dress apparel
            
            legging <- insert $ Product "Legging"
            insert $ ProductCategory legging apparel
            
            insert $ Product "Milkshake"
            -- no category
            
            return ()
            
        warpDebug 3000 $ App pool
