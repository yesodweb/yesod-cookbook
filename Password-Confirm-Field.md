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
    ((res, form), enctype) &lt;- runFormGet $ renderDivs $
  areq passwordConfirmField "Password" Nothing
    defaultLayout [whamlet|
&lt;p&gt;Result: #{show res}
&lt;form enctype=#{enctype}&gt;
    ^{form}
    &lt;p&gt;
  &lt;input type=submit&gt;
|]

passwordConfirmField :: Field sub master Text
passwordConfirmField = Field
    { fieldParse = \vals -&gt; do
  case vals of
      [x, y]
    | x == y -&gt; return $ Right $ Just x
    | otherwise -&gt; return $ Left "Passwords don't match"
      [] -&gt; return $ Right Nothing
      _ -&gt; return $ Left "Incorrect number of results"
    , fieldView = \id' name val isReq -&gt; [whamlet|
&lt;input ##{id'} name=#{name} type=password :isReq:required&gt;
&lt;p&gt;
    Confirm
    &lt;input ##{id'} name=#{name} type=password :isReq:required&gt;
|]
    }

main = warpDebug 3000 PC
```