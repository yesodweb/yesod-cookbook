# Raw JSON Parsing Example

The following code shows how to parse raw json in Yesod Handlers.

``` haskell
#!/usr/bin/env stack
{- stack
     --resolver lts-6.24
     --install-ghc
     runghc
     --package yesod
     --package yesod-core
     --package blaze-html
     --package text
     --package shakespeare
     --package aeson
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (join)
import Data.Maybe (isJust)
import Data.Text (Text, unpack)
import qualified Data.Text.Lazy.Encoding
import Data.Typeable (Typeable)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Hamlet (shamlet)
import Text.Shakespeare.Text (stext)
import Yesod
import Network.HTTP.Types.Status (status400, status200)
import Data.Aeson.Types (Result(..))
import Data.Aeson.Types

data App = App

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET POST
|]

instance Yesod App where
  approot = ApprootStatic "http://localhost:3006"
  
instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

data Response = Response {
      msg :: Text
} deriving (Show, Eq)

instance ToJSON Response where
    toJSON (Response x) = object [ "msg" .= x ]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  toWidget [hamlet|
            <p>Hello world
           |]


parseUser :: Value -> Parser (Text, Text)
parseUser = withObject "expected object" (\obj -> do
                                            uname <- obj .: "username"
                                            pword <- obj .: "password"
                                            return (uname, pword))

postHomeR :: Handler RepJson
postHomeR = do
  (jsobj :: Result Value) <- parseJsonBody
  case jsobj of
    Error err -> sendStatusJSON status400 (Response { msg = "invalid body"})
    Success v -> sendStatusJSON status200 (parseMaybe parseUser v)

main :: IO ()
main = warp 3000 App
```

Run the server and pass them curl request to see how they behave. Demo:

``` shellsession
sibi::casey { ~ }-> curl -H "Content-Type: application/json" -H "Accept: application/json" -X POST -d '{"username":"xyz","password":"xyz"}' http://127.0.0.1:3000
["xyz","xyz"]

sibi::casey { ~ }-> curl -H "Content-Type: application/json" -H "Accept: application/json" -X POST http://127.0.0.1:3006
{"msg":"invalid body"}
```

