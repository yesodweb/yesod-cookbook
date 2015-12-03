# Serving XHTML with Yesod

Yesod serves HTML by default. It can be made to serve XHTML. This page is a minimal example of how to serve XHTMl. It validates in https://validator.w3.org/

The biggest difference is the mimetype which is `application/xhtml+xml` for XHTML. Another difference is that XHTML should be valid XML. Hamlet does not enforce this. To get valid XHTML, use indentation in Hamlet instead of closing elements.

## xhtml-wrapper.hamlet:
```
\<?xml version='1.0' encoding='UTF-8'?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
  <head>
    <meta content="application/xhtml+xml;charset=utf-8" http-equiv="Content-Type"/>
    <meta content="en-US" http-equiv="Content-Language"/>
    <meta content="width=device-width, initial-scale=1.0, user-scalable=yes" name="viewport"/>

    <title>XHTML
  <body>
    <div .header>My lovely XHTML page.
    ^{pageBody pc}
    <div .footer>Created with Yesod.
```

## XHtmlServer.hs:
```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import Data.ByteString (ByteString)
import Text.Hamlet     (hamletFile)
import Yesod.Core

xhtmlType :: ByteString
xhtmlType = "application/xhtml+xml"

newtype Xhtml = Xhtml Html deriving ToContent
instance ToTypedContent Xhtml where
    toTypedContent = TypedContent xhtmlType . toContent

data App = App {}

instance Yesod App where
    defaultLayout widget = do
        pc <- widgetToPageContent widget
        withUrlRenderer $(hamletFile "xhtml-wrapper.hamlet")

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

getHomeR :: Handler Xhtml
getHomeR = do
    xhtml <- defaultLayout $ do
        [whamlet|
            <p>Hello XHTML
        |]
    return $ Xhtml xhtml

main :: IO ()
main = do
    warp 3000 App
```
