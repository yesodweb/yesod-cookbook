This demonstrates how to use WAI's ServerEvents together with the client-side code needed to enable full functionality of the feature. I've tried to make the code as tidy as possible, but in this as in much else YMMV. It is non-scaffolded to keep things simple and easily understandable.

**Tested with**:
* GHC 7.6.3, yesod-core-1.2.3, wai-1.4.0.1, warp-1.3.9
* Mozilla Firefox 22.0 (Debian testing), Chromium 24.0.1278.0 (Debian testing)

Point your browser to http://localhost:3000/setup after running the app, since there is no "/"-route in this example. 

```haskell
{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
  TemplateHaskell, FlexibleInstances, MultiParamTypeClasses,
  FlexibleContexts #-}

import Yesod
import Yesod.Form.Jquery
import Control.Concurrent.Chan (Chan, dupChan, writeChan, newChan)
import Control.Concurrent (forkIO, threadDelay)
import Data.Text (Text, pack)
import Text.Julius (rawJS)
import Blaze.ByteString.Builder.ByteString
import Blaze.ByteString.Builder.Char.Utf8 (fromText, fromString)
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppChan)
import Data.Monoid ((<>))
import Control.Monad.Trans.Resource (runResourceT)

data App = App (Chan ServerEvent)

pageTitleText = "EventSource Cookbook example" :: Text

mkYesod "App" [parseRoutes|
/recv ReceiveR GET
/setup SetupR GET
|]

instance Yesod App where
  defaultLayout widget = do
      appFoundation <- getYesod
      pageContent <- widgetToPageContent $ do
                       widget
                       addScriptEither (urlJqueryJs appFoundation)
      mmsg <- getMessage
      giveUrlRenderer [hamlet| $newline never
                       $doctype 5
                       <html>
                         <head>
                           <title>#{pageTitle pageContent}
                           ^{pageHead pageContent}
                         <body>
                           $maybe msg <- mmsg
                            <p .message>#{msg}
                           ^{pageBody pageContent}
                       |]

-- We want a more recent jQuery version than the default.
instance YesodJquery App where
    urlJqueryJs _ = Right $ "http://ajax.googleapis.com/"
                      <> "ajax/libs/jquery/1.8.3/jquery.min.js"

getReceiveR :: Handler TypedContent
getReceiveR = do
  App chan0 <- getYesod
  chan <- liftIO $ dupChan chan0
  req <- waiRequest
  res <- liftResourceT $ eventSourceAppChan chan req
  sendWaiResponse res

getSetupR :: Handler Html
getSetupR = do
  defaultLayout $ do
              setTitle $ toHtml pageTitleText
              eventSourceW

onlyEventName :: Text
onlyEventName = "ev1"

eventSourceW = do
  receptacle0 <- newIdent -- css id for output div 0
  receptacle1 <- newIdent -- css id for output div 1
  [whamlet| $newline never
            <div ##{receptacle0} .outdiv>^^ Unclassified output up here.
            <div ##{receptacle1} .outdiv>^^ Output 1 up here.|]
  -- the CSS for the above divs.
  toWidget [lucius|
           .outdiv
           {
             float:left;
             width:400px;
             font-family:courier,'courier new',sans-serif;
           }
         |]
  -- the JavaScript ServerEvent handling code
  toWidget [julius|
            // setup the EventSource itself
            var source = new EventSource('/recv');

            // listener for first type of events
            source.addEventListener(#{toJSON onlyEventName}, function(event)
              {
                $('##{rawJS receptacle1}')
                     .prepend(makeEventString('Events, type 1', event));
              }, false);

            // listener for unclassified events
            source.onmessage = function(event)
              {

                $('##{rawJS receptacle0}')
                     .prepend(makeEventString('Events, unclassified', event));
              };

            // just an output helper function
            function makeEventString(str, event)
              {
                return str + ': <strong>' +
                       event.data + ' </strong><br>';
              }
            |]

talk :: Chan ServerEvent -> Int -> IO ()
talk ch n = do
  writeChan ch $ mkServerEvent "" n "Foo! "
  threadDelay micros
  writeChan ch $ mkServerEvent onlyEventName n "Bar! "
  threadDelay micros
  talk ch (n+1)
    where micros = 1*(10^6)
          mkServerEvent evName evId evData =
              let mEvName = case evName of
                                 "" -> Nothing
                                 _  -> (Just $ fromText evName)
                  mEvId   = Just $ fromString $ show evId
                  evPayload = [(fromText evData <> fromString (show evId))]
              in ServerEvent mEvName mEvId evPayload

main = do
    ch <- newChan
    forkIO $ talk ch 0
    warp 3000 $ App ch
```