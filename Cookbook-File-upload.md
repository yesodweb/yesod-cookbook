This example was orginal posted by Michael Snoyman, see [http://permalink.gmane.org/gmane.comp.lang.haskell.yesod/323](http://permalink.gmane.org/gmane.comp.lang.haskell.yesod/323)

See also the extended cookbook [[File upload saving files to server|Cookbook/File upload saving files to server]].

    {-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell,
             TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}
    import Yesod
    import Control.Applicative
    
    data File = File
    
    instance Yesod File where
        approot = ApprootStatic ""
    instance RenderMessage File FormMessage where
        renderMessage _ _ = defaultFormMessage
    
    mkYesod "File" [parseRoutes|
    / RootR GET POST
    |]
    
    form :: Html -> MForm File File (FormResult (FileInfo, Maybe FileInfo), Widget)
    form = renderDivs $ (,) <$> fileAFormReq "File" <*> fileAFormOpt "Optional file"
    
    getRootR :: Handler RepHtml
    getRootR = do
        ((_, widget), enctype) <- runFormPost form
        defaultLayout [whamlet|$newline never
    <form method=post enctype=#{enctype}>
        ^{widget}
        <p>
        <input type=submit>
    |]
    
    postRootR :: Handler RepHtml
    postRootR = do
        ((result, widget), enctype) <- runFormPost form
        let msubmission = case result of
                FormSuccess res -> Just res
                _ -> Nothing
        defaultLayout $ do
            [whamlet|$newline never
    $maybe (firstfile, second) <- msubmission
        <p>File received: #{fileName firstfile}
        $maybe secondfile <- second
            <p>Second file received: #{fileName secondfile}
    <form method=post enctype=#{enctype}>
        ^{widget}
        <p>
        <input type=submit>
    |]
    
    main :: IO ()
    main = warpDebug 3000 File