```haskell
{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell,
       TypeFamilies, MultiParamTypeClasses #-}
import Yesod
import Data.Text (Text)
import Control.Applicative ((<$>), (<*>))

data MF = MF

instance Yesod MF
instance RenderMessage MF FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesod "MF" [parseRoutes|
/ RootR GET
|]

form html = do
    (res, widget) <- flip renderDivs html $ (,)
            <$> areq textField "Name" Nothing
            <*> areq intField "Age" Nothing
    return $ case res of
              FormSuccess (name, age)
                        | name == "Michael" && age /= 26 ->
                      let msg = "Invalid name/age combination"
                       in (FormFailure [msg], [whamlet|
                  <p .errors>#{msg}
                  ^{widget}
                  |])
              _ -> (res, widget)

getRootR :: Handler Html
getRootR = do
    ((res, widget), enctype) <- runFormGet form
    defaultLayout [whamlet|
<p>Result: #{show res}
<form enctype=#{enctype}>
    ^{widget}
    <p>
  <input type=submit>
|]

main = warp 3000 MF
```