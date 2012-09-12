Static subdomains are also provided in the scaffolded site.

    {-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
    import Yesod
    import Yesod.Static
    import Network.Wai.Handler.Warp (run)
    import Network.Wai.Middleware.Vhost (vhost)
    import Network.Wai.Application.Static (staticApp)
    import Network.Wai (serverName)
    
    staticFiles "static"
    
    -- Standard setup for a project using a static subsite
    data SD = SD
        { getStatic :: Static
        }
    
    mkYesod "SD" [parseRoutes|
    / RootR GET
    /static StaticR Static getStatic
    |]
    
    instance Yesod SD where
        -- Approot contains the main domain name
        approot = ApprootStatic "http://localhost:3000"
    
        -- Set up urlRenderOverride to point to the static domain name
        urlRenderOverride a (StaticR s) = Just $ uncurry (joinPath a "http://127.0.0.1:3000") $ renderRoute s
        urlRenderOverride _ _ = Nothing
    
    getRootR :: Handler RepHtml
    getRootR = defaultLayout [whamlet|$newline never
    <img src=@{StaticR image_png} />|]
    
    main :: IO ()
    main = do
        -- Get the static subsite, as well as the settings it is based on
        s@(Static settings) <- static "static"
    
        -- Generate the main application
        app <- toWaiApp $ SD s
    
        -- Serve a virtual host with Warp, using the static application for the
        -- static domain name
        run 3000 $ vhost
            [ ((== "127.0.0.1") . serverName, staticApp settings)
            ] app
