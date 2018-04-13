## Render an HTML file from an Handler

Main idea: Using the `preEscapedText` from the package `blaze-markup`

``` haskell
#!/usr/bin/env stack
{- stack
     --resolver lts-11.4
     --install-ghc
     runghc
     --package yesod
     --package text
 -}


{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import Yesod
import Data.Text
import qualified Data.Text.IO as TIO

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod HelloWorld

htmlFilePath :: FilePath
htmlFilePath = "/home/sibi/hello.html"

getHomeR :: Handler Html
getHomeR = do
  htmlContent <- liftIO $ TIO.readFile htmlFilePath
  return $ preEscapedToMarkup htmlContent

main :: IO ()
main = warp 3000 HelloWorld
```
