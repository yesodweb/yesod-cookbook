```haskell
{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell,
       TypeFamilies, MultiParamTypeClasses #-}
import Yesod
import Data.Text (Text)

data PC = PC

instance Yesod PC where
    approot _ = ""
instance RenderMessage PC FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesod "PC" [parseRoutes|
/ RootR GET
|]

getRootR :: Handler RepHtml
getRootR = do
    ((res, form), enctype) <- runFormGet $ renderDivs $
  areq passwordConfirmField "Password" Nothing
    defaultLayout [whamlet|
<p>Result: #{show res}
<form enctype=#{enctype}>
    ^{form}
    <p>
  <input type=submit>
|]

passwordConfirmField :: Field sub master Text
passwordConfirmField = Field
    { fieldParse = \vals -> do
  case vals of
      [x, y]
    | x == y -> return $ Right $ Just x
    | otherwise -> return $ Left "Passwords don't match"
      [] -> return $ Right Nothing
      _ -> return $ Left "Incorrect number of results"
    , fieldView = \id' name val isReq -> [whamlet|
<input ##{id'} name=#{name} type=password :isReq:required>
<p>
    Confirm
    <input ##{id'} name=#{name} type=password :isReq:required>
|]
    }

main = warpDebug 3000 PC
```