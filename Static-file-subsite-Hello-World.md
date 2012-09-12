The simplest way to serve static files. This is a clean example without the other features in the scaffolded site.

    {-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
    import Yesod
    import Yesod.Static
    
    staticFiles "static"
    
    data App = App
        { getStatic :: Static
        }
    
    mkYesod "App" [parseRoutes|
    / RootR GET
    /static StaticR Static getStatic
    |]
    
    instance Yesod App
    
    getRootR :: Handler RepHtml
    getRootR = defaultLayout [whamlet|$newline never
    <img src=@{StaticR image_png}/>|]
    
    main :: IO ()
    main = do
        -- Get the static subsite, as well as the settings it is based on
        static@(Static settings) <- static "static"
        warpDebug 3000 $ App static
