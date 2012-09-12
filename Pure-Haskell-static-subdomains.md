Q. Can anyone provide a simple example of running a warp serving static pages from the same code as main application. I want to get rid of nginx, but also do not want sessions overhead on static pages.

A. To use different ports, you'd instead want to have two Warp processes running and skip the vhost. I kept the example with vhost because it's the recommended approach.

To make this example run, create a folder "static" and save a file "image.png" in it.

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