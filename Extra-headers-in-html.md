
The following the pages are almost the same. Yesod-book has a hamletToRepHtml on the example (in the Widgets-chapter) while the defaultLayout in the root-page below adds an extra doctype and head into the generated html. The outputs are in the bottom.

    {-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses #-}
    import Yesod
    import Control.Applicative
    import Data.Text (Text)
    import Text.Hamlet

    data Example = Example
    
    mkYesod "Example" [parseRoutes|
    / RootR GET
    /page PageR GET
    /page2 Page2R GET
    |]
    
    instance Yesod Example
     
    getRootR :: GHandler sub Example RepHtml
    getRootR = do
        defaultLayout [whamlet|
    $doctype 5
    <html>
      <head>
        <title>Tutorial, hello root
        <body>
        <h1 id="greeting">Hello root
    |]
    
    getPageR :: GHandler sub Example RepHtml
    getPageR = defaultLayout $ do
        toWidgetHead [hamlet| <meta charset="utf-8"> |]
        setTitle "hello page"
        toWidget [hamlet|
    <h1 id ="greetings2">Hello page
    |]

    getPage2R :: GHandler sub Example RepHtml
    getPage2R = hamletToRepHtml [hamlet| 
    $doctype 5
    <html>
        <head>
            <title>Tutorial, hello page2
        <body>
            <h1 id="greeting">Hello page2
    |]
   
    main :: IO ()
    main = warpDebug 3000 Example

 Html-output of the "root"-page is

    <!DOCTYPE html>
    <html><head><title></title></head><body><!DOCTYPE html>
    <html><head><title>Tutorial, hello page2</title>
    </head>
    <body><h1 id="greeting">Hello page2</h1>
    </body>
    </html>
    </body></html>

and the output of "page" is

    <!DOCTYPE html>
    <html><head><title>hello page</title><meta charset="utf-8"> </meta>
    </head><body><h1 id="greetings2">Hello page</h1>
    </body></html>

and the output of "page2" is

    <!DOCTYPE html>
    <html><head><title>Tutorial, hello page2</title>
    </head>
    <body><h1 id="greeting">Hello page2</h1>
    </body>
    </html>
