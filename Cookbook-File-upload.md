This example was orginal posted by Michael Snoyman, see [http://permalink.gmane.org/gmane.comp.lang.haskell.yesod/323](http://permalink.gmane.org/gmane.comp.lang.haskell.yesod/323)


	{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell,
		     TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}
	import Yesod
	import Control.Applicative
	 
	data File = File
	 
	instance Yesod File where
	    approot _ = ""
	instance RenderMessage File FormMessage where
	    renderMessage _ _ = defaultFormMessage
	 
	mkYesod "File" [parseRoutes|
	/ RootR GET POST
	|]
	 
	form :: Html -> Form File File (FormResult (FileInfo, Maybe FileInfo), Widget)
	form = renderDivs $ (,) <$> fileAFormReq "File" <*> fileAFormOpt "Optional file"
	 
	getRootR :: Handler RepHtml
	getRootR = do
	    ((_, widget), enctype) <- runFormPost form
	    defaultLayout [whamlet|
	<form method=post enctype=#{enctype}>
	    ^{widget}
	    <p>
		<input type=submit>
	|]
	 
	postRootR :: Handler RepHtml
	postRootR = do
	    ((res, widget), enctype) <- runFormPost form
	    defaultLayout $ case res of
		FormSuccess x -> [whamlet|<p>#{show x}|]
		_ -> [whamlet|
	<form method=post enctype=#{enctype}>
	    ^{widget}
	    <p>
		<input type=submit>
	|]
	 
	main :: IO ()
	main = warpDebug 3000 File


***

This code does not run out of the box. The compiler complains about approot and Form (Not in scope: type constructor or class `Form').