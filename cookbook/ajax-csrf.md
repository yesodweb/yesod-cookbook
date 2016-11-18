# Doing AJAX calls with CSRF Protection

This is a nice [video](https://www.youtube.com/watch?v=hW2ONyxAySY)
which describes CSRF and how it can be exploited. Yesod offers easy to
use protection again CSRF by providin a middleware for it. For using
it, all you have to do is include it while making the instance of it

```
yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware
```

A sample demo in which we use the csrf middleware in our yesod
application:

``` haskell
#!/usr/bin/env stack
{- stack
     --resolver lts-6.24
     --install-ghc
     runghc
     --package yesod
     --package yesod-core
     --package yesod-auth
     --package mime-mail
     --package blaze-html
     --package persistent
     --package text
     --package aeson
     --package persistent-sqlite
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
import Control.Monad.Logger (runNoLoggingT)
import Data.Maybe (isJust)
import Data.Text (Text, unpack)
import qualified Data.Text.Lazy.Encoding
import Data.Typeable (Typeable)
import Database.Persist.Sqlite
import Database.Persist.TH
import Network.Mail.Mime
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Hamlet (shamlet)
import Text.Shakespeare.Text (stext)
import Yesod
import Yesod.Auth
import Yesod.Auth.Email
import Data.Aeson.Types

share
  [ mkPersist
      sqlSettings
  , mkMigrate "migrateAll"
  ]
  [persistLowerCase|
User json
    email String
    verified Bool
    deriving Show
|]

data App =
  App SqlBackend

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
/json/test TestR POST
|]

instance Yesod App where
  approot = ApprootStatic "http://localhost:3006"
  
  yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB f = do
    App conn <- getYesod
    runSqlConn f conn

postTestR :: Handler TypedContent
postTestR = do
  user' :: User <- requireJsonBody
  runDB $ insert_ user'
  selectRep $ provideRep $ return emptyObject

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.4/jquery.js"
  toWidget
        [julius|
            $(function() {
                $("#btn").click(function(){
                            var dummyUser = {
                                "email": "sibi@psibi.in",
                                "verified": true
                            };
                            $.ajax({
                               url: '/json/test',
                               contentType: 'application/json',
                               type: 'post',
                               dataType: 'json',
                               success: function (data) {
                                  console.log('success', data);
                               },             
                               data: JSON.stringify(dummyUser)
                             });
                });
                });
        |]
  toWidget [hamlet|
            <p>Hello world
            <button #btn>Send POST request       
           |]

main :: IO ()
main =
  runNoLoggingT $
  withSqliteConn ":memory:" $
  \conn ->
     liftIO $
     do runSqlConn (runMigration migrateAll) conn
        warp 3006 $ App conn
```

But the above code won't work, because we are not passing proper csrf
token in the ajax call. In fact, when you make the AJAX call, it will
give out this response:

``` json
{
"message":"Permission Denied. A valid CSRF token wasn't present in HTTP headers or POST parameters.
 Because the request could have been forged, it's been rejected altogether. Check the Yesod.Core.Handler
 docs of the yesod-core package for details on CSRF protection."
}
```

To make it work, you have to fix up the AJAX call being made. A Working
code with CSRF protection:

```
#!/usr/bin/env stack
{- stack
     --resolver lts-6.24
     --install-ghc
     runghc
     --package yesod
     --package yesod-core
     --package yesod-auth
     --package mime-mail
     --package blaze-html
     --package persistent
     --package text
     --package aeson
     --package persistent-sqlite
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
import Control.Monad.Logger (runNoLoggingT)
import Data.Maybe (isJust)
import Data.Text (Text, unpack)
import qualified Data.Text.Lazy.Encoding
import Data.Typeable (Typeable)
import Database.Persist.Sqlite
import Database.Persist.TH
import Network.Mail.Mime
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Hamlet (shamlet)
import Text.Shakespeare.Text (stext)
import Yesod
import Yesod.Auth
import Yesod.Auth.Email
import Data.Aeson.Types
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Text.Julius (rawJS)
import Yesod.Core.Handler (defaultCsrfCookieName, defaultCsrfHeaderName)

share
  [ mkPersist
      sqlSettings
  , mkMigrate "migrateAll"
  ]
  [persistLowerCase|
User json
    email String
    verified Bool
    deriving Show
|]

data App =
  App SqlBackend

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
/json/test TestR POST
|]

instance Yesod App where
  approot = ApprootStatic "http://localhost:3006"
  
  yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

-- Set up Persistent
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB f = do
    App conn <- getYesod
    runSqlConn f conn

data SampleUser = SampleUser {
      suEmail :: String,
      suVerified :: Bool
} deriving (Show, Eq, Ord)

instance FromJSON SampleUser where
    parseJSON (Object v) = SampleUser <$>
                          v .: "email" <*>
                          v .: "verified"
    parseJSON _ = error "parsing error"

postTestR :: Handler TypedContent
postTestR = do
  user' :: SampleUser <- requireJsonBody
  liftIO $ print user'
  runDB $ insert_ (User (suEmail user') (suVerified user'))
  selectRep $ provideRep $ return emptyObject

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.4/jquery.js"
  addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/js-cookie/2.0.3/js.cookie.min.js"
  let hname = rawJS $ TE.decodeUtf8 $ CI.foldedCase defaultCsrfHeaderName
      cname = rawJS $ TE.decodeUtf8 defaultCsrfCookieName
  toWidget
        [julius|
            $(function() {
                var csrfCookieName = "#{cname}";
                $("#btn").click(function(){
                            var dummyUser = {
                                "email": "sibi@psibi.in",
                                "verified": true
                            };
                            $.ajax({
                               url: '/json/test',
                               contentType: 'application/json',
                               type: 'post',
                               headers: {
                                "#{hname}": Cookies.get(csrfCookieName)
                               },
                               dataType: 'json',
                               success: function (data) {
                                  console.log('success', data);
                               },             
                               data: JSON.stringify(dummyUser)
                             });
                });
                });
        |]
  toWidget [hamlet|
            <p>Hello world
            <button #btn>Send POST request
           |]

main :: IO ()
main =
  runNoLoggingT $
  withSqliteConn ":memory:" $
  \conn ->
     liftIO $
     do runSqlConn (runMigration migrateAll) conn
        warp 3006 $ App conn
```
