# Demo of Yesod.Auth.Email module using JSON endpoints

The code below will show how to get the JSON endpoints
of
[Yesod.Auth.Email](https://www.stackage.org/haddock/nightly-2016-12-08/yesod-auth-1.4.15/Yesod-Auth-Email.html) module
to interact. Note that you need a minimum version of
`yesod-auth-1.4.15` for this code to work. The different buttons in
the home page will invoke a AJAX request and will communicate with the
Email Authentication module.

``` haskell
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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
import Text.Julius (rawJS)
import Text.Shakespeare.Text (stext)
import Yesod
import Yesod.Auth
import Yesod.Auth.Email
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

share
  [ mkPersist
      sqlSettings
      { mpsGeneric = False
      }
  , mkMigrate "migrateAll"
  ]
  [persistLowerCase|
User
    email Text
    password Text Maybe -- Password may not be set yet
    verkey Text Maybe -- Used for resetting passwords
    verified Bool
    UniqueUser email
    deriving Typeable
|]

data App =
  App SqlBackend

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
/auth AuthR Auth getAuth
|]

instance Yesod App
         -- Emails will include links, so be sure to include an approot so that
         -- the links are valid!
                                 where
  approot = ApprootStatic "http://localhost:3005"
  yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

-- Set up Persistent
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB f = do
    App conn <- getYesod
    runSqlConn f conn

instance YesodAuth App where
  type AuthId App = UserId
  loginDest _ = HomeR
  logoutDest _ = HomeR
  authPlugins _ = [authEmail]
  -- Need to find the UserId for the given email address.
  getAuthId creds =
    runDB $
    do x <- insertBy $ User (credsIdent creds) Nothing Nothing False
       return $
         Just $
         case x of
           Left (Entity userid _) -> userid -- newly added user
           Right userid -> userid -- existing user
  authHttpManager = error "Email doesn't need an HTTP manager"

instance YesodAuthPersist App

-- Here's all of the email-specific code
instance YesodAuthEmail App where
  type AuthEmailId App = UserId
  afterPasswordRoute _ = HomeR
  addUnverified email verkey =
    runDB $ insert $ User email Nothing (Just verkey) False
  sendVerifyEmail email _ verurl
                          -- Print out to the console the verification email, for easier
                          -- debugging.
   = do
    liftIO $ putStrLn $ "Copy/ Paste this URL in your browser:" ++ unpack verurl
    -- Send email.
    liftIO $
      renderSendMail
        (emptyMail $ Address Nothing "noreply")
        { mailTo = [Address Nothing email]
        , mailHeaders = [("Subject", "Verify your email address")]
        , mailParts = [[textPart, htmlPart]]
        }
    where
      textPart =
        Part
        { partType = "text/plain; charset=utf-8"
        , partEncoding = None
        , partFilename = Nothing
        , partContent =
          Data.Text.Lazy.Encoding.encodeUtf8
            [stext|
                    Please confirm your email address by clicking on the link below.

                    #{verurl}

                    Thank you
                |]
        , partHeaders = []
        }
      htmlPart =
        Part
        { partType = "text/html; charset=utf-8"
        , partEncoding = None
        , partFilename = Nothing
        , partContent =
          renderHtml
            [shamlet|
                    <p>Please confirm your email address by clicking on the link below.
                    <p>
                        <a href=#{verurl}>#{verurl}
                    <p>Thank you
                |]
        , partHeaders = []
        }
  getVerifyKey = runDB . fmap (join . fmap userVerkey) . get
  needOldPassword _ = return False
  setVerifyKey uid key = runDB $ update uid [UserVerkey =. Just key]
  verifyAccount uid =
    runDB $
    do mu <- get uid
       case mu of
         Nothing -> return Nothing
         Just u -> do
           update uid [UserVerified =. True, UserVerkey =. Nothing]
           return $ Just uid
  getPassword = runDB . fmap (join . fmap userPassword) . get
  setPassword uid pass = runDB $ update uid [UserPassword =. Just pass]
  getEmailCreds email =
    runDB $
    do mu <- getBy $ UniqueUser email
       case mu of
         Nothing -> return Nothing
         Just (Entity uid u) ->
           return $
           Just
             EmailCreds
             { emailCredsId = uid
             , emailCredsAuthId = Just uid
             , emailCredsStatus = isJust $ userPassword u
             , emailCredsVerkey = userVerkey u
             , emailCredsEmail = email
             }
  getEmail = runDB . fmap (fmap userEmail) . get

getHomeR :: Handler Html
getHomeR = do
  maid <- maybeAuthId
  defaultLayout $
    do addScriptRemote
         "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.4/jquery.js"
       addScriptRemote
         "https://cdnjs.cloudflare.com/ajax/libs/js-cookie/2.0.3/js.cookie.min.js"
       let hname = rawJS $ TE.decodeUtf8 $ CI.foldedCase defaultCsrfHeaderName
           cname = rawJS $ TE.decodeUtf8 defaultCsrfCookieName
       toWidget
         [julius|
                     $(function() {
                     var csrfCookieName = "#{cname}";
                     var hname = "#{hname}";
                     console.log(csrfCookieName, hname);

                $("#btn").click(function(){
                            var dummyData = {
                                "email": "sibi@psibi.in"
                            };
                            $.ajax({
                               url: '/auth/page/email/register',
                               contentType: 'application/json',
                               type: 'post',
                               headers: {
                                "#{hname}": Cookies.get(csrfCookieName)
                               },
                               dataType: 'json',
                               success: function (data) {
                                  console.log('success', data);
                               },
                               data: JSON.stringify(dummyData)
                             });
                });

                $("#btn2").click(function(){
                            var dummyData = {
                                "email": "sibi@psibi.in"
                            };
                            $.ajax({
                               url: '/auth/page/email/forgot-password',
                               contentType: 'application/json',
                               type: 'post',
                               headers: {
                                "#{hname}": Cookies.get(csrfCookieName)
                               },
                               dataType: 'json',
                               success: function (data) {
                                  console.log('success', data);
                               },
                               data: JSON.stringify(dummyData)
                             });
                });

                $("#btn3").click(function(){
                            var dummyData = {
                                "email": "sibi@psibi.in",
                                "password": "sibi"
                            };
                            $.ajax({
                               url: '/auth/page/email/login',
                               contentType: 'application/json',
                               type: 'post',
                               headers: {
                                "#{hname}": Cookies.get(csrfCookieName)
                               },
                               dataType: 'json',
                               success: function (data) {
                                  console.log('success', data);
                               },
                               data: JSON.stringify(dummyData)
                             });
                });

                $("#btn4").click(function(){
                            var dummyData = {
                                "new": "sibi",
                                "confirm": "sibi",
                                "current": "sibi"
                            };
                            $.ajax({
                               url: '/auth/page/email/set-password',
                               contentType: 'application/json',
                               type: 'post',
                               headers: {
                                "#{hname}": Cookies.get(csrfCookieName)
                               },
                               dataType: 'json',
                               success: function (data) {
                                  console.log('success', data);
                               },
                               data: JSON.stringify(dummyData)
                             });
                });

                     });
             |]
       toWidget
         [whamlet|
            <p>Your current auth ID: #{show maid}
            <button #btn>Register user
            <button #btn2>Forgot password
            <button #btn3>Login
            <button #btn4>Set password

            $maybe _ <- maid
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>Go to the login page
        |]

main :: IO ()
main =
  runNoLoggingT $
  withSqliteConn "email.db3" $
  \conn ->
     liftIO $
     do runSqlConn (runMigration migrateAll) conn
        warp 3005 $ App conn
```



