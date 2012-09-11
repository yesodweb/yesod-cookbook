This is a simple, single-page example of using hierarchical routes. Note that if you are using a scaffolded site, you should export the hierarchical route. For example (in Foundation.hs):
```
module Foundation
    ( App (..)
    , Route (..)
    , AppMessage (..)
    , NLR (..)
    , UKR (..)
    , AdminR (..)
```

In the single-page app exporting is not necessary.

The following code also shows how to use breadcrumbs.

    {-# LANGUAGE QuasiQuotes
               , TypeFamilies
               , MultiParamTypeClasses
               , TemplateHaskell
               , OverloadedStrings #-}
    import Yesod
    
    data App = App
    
    mkYesod "App" [parseRoutes|
    / RootR GET
    
    /nl NLR:
        / NLRootR GET
    
    /uk UKR:
        / UKRootR GET
    
    /admin AdminR:
        / AdminRootR GET
    |]
    
    instance Yesod App where
        defaultLayout = myLayout
    
    instance YesodBreadcrumbs App where
        breadcrumb RootR = return ("Home", Nothing)
        breadcrumb (NLR NLRootR) = return ("NL", Just RootR)
        breadcrumb (UKR UKRootR) = return ("UK", Just RootR)
        breadcrumb (AdminR AdminRootR) = return ("Admin", Just RootR)
    
    getRootR :: Handler RepHtml
    getRootR = defaultLayout [whamlet|$newline never
    <h1>Homepage
    <p>
        Quick links: #
        <a href=@{NLR NLRootR}>
            NL subsite
        \, #
        <a href=@{UKR UKRootR}>
            UK subsite
        \ and #
        <a href=@{AdminR AdminRootR}>
            admin pages
        \.
    |]
    
    getAdminRootR :: Handler RepHtml
    getAdminRootR = do
        defaultLayout $ do
        [whamlet|$newline never
    <h1>Admin
    |]
    
    getNLRootR :: Handler RepHtml
    getNLRootR = do
        defaultLayout $ do
        [whamlet|$newline never
    <h1>NL
    |]
    
    getUKRootR :: Handler RepHtml
    getUKRootR = do
        defaultLayout $ do
        [whamlet|$newline never
    <h1>UK
    |]
    
    myLayout :: GWidget s App () -> GHandler s App RepHtml
    myLayout widget = do
        (title, parents) <- breadcrumbs
        pc <- widgetToPageContent widget
        hamletToRepHtml [hamlet|$newline never
    $doctype 5
    <html>
        <head>
            <title>#{pageTitle pc}
            <meta charset=utf-8>
            <style>
                body {
                    font-family: sans-serif;
                }
                #breadcrumbs {
                    font-size: small;
                }
                #breadcrumbs span {
                    margin: 0 .5em 0 0;
                }
            ^{pageHead pc}
        <body>
            <nav #breadcrumbs>
                <span>
                    You are here:#
                $forall bc <- parents
                    <span>
                        <a href=@{fst bc}>
                            #{snd bc}
                        \ &gt;
                <span>
                    #{title}
            <article>
                ^{pageBody pc}
    |]
    
    main = warpDebug 3000 $ App