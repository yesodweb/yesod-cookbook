	{-
	
	INTERNATIONALIZED FORM
	
	Below is an example of how to internationalize a Yesod application,
	including internationalization (i18n) of a form and Yesod's built-in
	form error messages.
	
	In the example we create a simple site that will show messages in
	Swedish or English depending on the browser's preferred language.
	If neither Swedish or English are preferred languages the site will
	default to Swedish.
	
	Familiarity with Yesod at the level of the Yesod book is assumed,
	comments are concentrated to the parts relevant to i18n.
	
	-}
	
	{-# LANGUAGE QuasiQuotes
		   , TemplateHaskell
		   , MultiParamTypeClasses
		   , OverloadedStrings
		   , TypeFamilies
	  #-}
	
	import Yesod
	import Yesod.Form.I18n.Swedish
	import Control.Applicative ((<$>), (<*>))
	import Data.Text (Text)
	
	
	-- Our foundation data type.
	data MyApp = MyApp
	
	mkYesod "MyApp" [parseRoutes|
	/    RootR GET
	|]
	
	instance Yesod MyApp where
	    approot _ = ""
	
	
	-- Here we provide internationalization of Yesod's built in form
	-- messages (mostly error messages). For brevity it is assumed that
	-- Swedish translations are exported as 'swedishFormMessage' by the
	-- module Yesod.Form.I18n.Swedish (an implementation can be copied
	-- from https://gist.github.com/1209328).
	instance RenderMessage MyApp FormMessage where
	    renderMessage _ []        = swedishFormMessage -- Default to Swedish
	    renderMessage _ ("sv":ls) = swedishFormMessage
	    renderMessage _ ("en":ls) = defaultFormMessage -- English
	    renderMessage m (_   :ls) = renderMessage m ls
	
	-- Next we define the custom messages present on the site and their
	-- rendering functions for different languages.
	data Msg = Model
		 | Year
		 | Please
	
	-- Rendering function for English.
	renderEnglish Model  = "Model"
	renderEnglish Year   = "Year"
	renderEnglish Please = "Please fill in your car's details"
	
	-- Rendering function for Swedish.
	renderSwedish Model  = "Modell"
	renderSwedish Year   = "Årgång"
	renderSwedish Please = "Vänligen fyll i uppgifterna för din bil"
	
	-- The instance used to select the appropriate rendering function.
	-- This is almost identical to the instance for FormMessage above.
	instance RenderMessage MyApp Msg where
	    renderMessage _ []        = renderSwedish -- Default to Swedish
	    renderMessage _ ("sv":ls) = renderSwedish
	    renderMessage _ ("en":ls) = renderEnglish
	    renderMessage m (_   :ls) = renderMessage m ls
	
	
	-- The data model.
	data Car = Car
	    { carModel :: Text
	    , carYear :: Int
	    }
	  deriving Show
	
	-- In our form we use our messages Model and Year as field labels.
	carAForm :: AForm MyApp MyApp Car
	carAForm = Car
	    <$> areq textField (fs Model) Nothing
	    <*> areq intField  (fs Year)  Nothing
	    where
		fs msg = FieldSettings msg Nothing Nothing Nothing
	
	carForm :: Html -> Form MyApp MyApp (FormResult Car, Widget)
	carForm = renderTable carAForm
	
	
	-- Our handler just shows the form, with submitted values pre-filled.
	-- Here we also use the Please message.
	getRootR :: Handler RepHtml
	getRootR = do
	    ((_, widget), enctype) <- runFormGet carForm
	    defaultLayout [whamlet|
		<p>_{Please}
		<form method=get action=@{RootR} enctype=#{enctype}>
		    <table>
			^{widget}
		    <p><input type=submit>
		|]
	
	
	-- | Launch the app on port 3000.
	main :: IO ()
	main = warpDebug 3000 MyApp
