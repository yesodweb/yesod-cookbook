# Using Ajax with a Scaffolded Site

A common scenario is that part of an application requires Ajax, while the rest is based on dynamically generated HTML.  The core functionality of Yesod includes everything you need to implement this, but the scaffolding leans more towards the HTML side, especially in its handling of login and its reporting of errors.  However, the default functions can easily be overridden with more general ones which handle Ajax properly.  This article explains how.

In what follows, I look at

* Customising error handling to avoid sending HTML error pages in response to an Ajax request.
* Customising authentication to avoid or mitigate redirections when responding to an Ajax request.
* Testing using Yesod.Test.
* An example handler and client-side code, to illustrate the choices on which the rest of the discussion is based.

## A caveat

To give concrete examples, I have had to make choices about the way Ajax calls are done.  To be specific, I use *jQuery* in the about the simplest way possible, which means that:

* Data is sent to the server as a set of form fields, with one parameter for each top-level attribute of the JavaScript object being sent.
* The response can be text or JSON, as long as it is consistent with the setting of the *dataType* in the Ajax call on the client side.
* For an error response, with an HTTP error status, reading the body as text is an easy option for the client.
* The requests include a header *"X-Requested-With: XMLHttpRequest"*.

You may want to make different choices, such as sending JSON data in all requests and responses.  The details will change, but the main ponts I am talking about here will remain valid.

An example Yesod handler, and the corresponding client-side code, are given in the appendix at the end to illustrate these choices,  and generally motivate the discussion.

I should also mention that issues [#478](https://github.com/yesodweb/yesod/issues/478) and [#479](https://github.com/yesodweb/yesod/issues/479) on GitHub discuss related issues, but not really the detailed scenario I have chosen here.

## Error Handler

The first problem to look at is error handling.  The example code shown later can raise a 404 error if the database entry is not found at all, and a *permissionDenied* error if the user is trying to use someone else's clipboard.  Errors like these short-circuit the handler and return a result using the *errorHandler* function of the Yesod instance.

The scaffolded site is set up to use the default error handler.  This produces a brief HTML message in a page formatted by *defaultLayout*, and is probably not what we want in response to an Ajax request!

To change it, we need to implement the *errorHandler* method of the Yesod typeclass, rather than using the default setting.  It is normal Handler code, and takes an ErrorResponse (see the documentation for Yesod.Handler), so we need to write something like this in Foundation.hs:

```haskell
import Network.HTTP.Types (mkStatus)
import Network.Wai (Request(..))
import Data.Text (append, pack, unwords)
import Control.Monad (when)
...
instance Yesod App where
    ...
    errorHandler errorResponse = do
        $(logWarn) (append "Error Response: " $ pack (show errorResponse))
        req <- waiRequest
        let reqwith = lookup "X-Requested-With" $ requestHeaders req
            errorText NotFound = (404, "Not Found", "Sorry, not found")
            errorText (InternalError msg) = (400, "Bad Request", msg)
            errorText (InvalidArgs m) = (400, "Bad Request", unwords m)
            errorText (PermissionDenied msg) = (403, "Forbidden", msg)
            errorText (BadMethod _) = (405, "Method Not Allowed",
                                            "Method not supported")
        when (maybe False (== "XMLHttpRequest") reqwith) $ do
            let (code, brief, full) = errorText errorResponse
            sendResponseStatus
                (mkStatus code brief)
                $ RepPlain $ toContent $ append "Error: " full
        defaultErrorHandler errorResponse
```

The critical part of this is recognising the Ajax call by the *X-Requested-With* header.  If you are using a different Ajax setup, you might need to look for something different, of course, but this one works for jQuery.

In the Ajax case, we produce a plain text response, which is what I chose as easiest in our example scenario - if you prefer JSON, then feel free to produce it here.  If the request is *not* Ajax, we call the *defaultErrorHandler*, which is what would have been used if we had not done any of this.

If you have been watching, you will have noticed that I also sneaked in some logging - it is entirely optional, but I think it is helpful.

## Login

The other issue which arises when working with Ajax is the way redirection occurs when a user needs to log in during an Ajax request.

The first thing that happens is that a redirection is done to the login page.  This uses a 303 status code, and so cannot be trapped in JavaScript - the browser itself automatically follows it.  So we *will* be redirected to a login page.  More precisely, we arrive at the *loginHandler* of the YesodAuth instance.  Our problem, just as in the error handler, is that we do not want it to generate an HTML login form if this happens when handling an Ajax request.

Therefore, in the YesodAuth instance, we implement the *loginHandler*, rather than accepting the default, again in Foundation.hs.

```haskell
instance YesodAuth App where
    ....
    loginHandler = do
        tm <- getRouteToMaster
        master <- getYesod
        clearUltDest
        req <- waiRequest
        let reqwith = lookup "X-Requested-With" $ requestHeaders req
        when (maybe False (== "XMLHttpRequest") reqwith) $ do
            sendResponseStatus
                (mkStatus 403 "Forbidden")
                $ RepPlain $ toContent ("Login required" :: Text)
        let title = renderMessage master ["en"] MsgSiteTitle
        defaultLayout $ do
            setTitleI title
            mapM_ (flip apLogin tm) (authPlugins master)
```

As before, we recognise an Ajax request from the *X-Requested-With* header.  For Ajax we produce an error instead of a login page, leaving the client-side code to decide how to tidy up and get the user to a place where they can log in.

There are a few subtleties here.  Firstly, we use a 403 response in the Ajax case.  401 would not be a good choice, since it would be an invitation to do HTTP authentication, not session-based login.  However 403 has the complication that it also gets used for other *permissionDenied* errors (see above).  This is resolved by taking a little care to ensure that the two cases are unambiguously distinguished by the start of the message, so that the client code can handle them appropriately.

The next subtlety concerns the "ultimate destination" - see the [Sessions chapter of the Yesod Book](http://www.yesodweb.com/book/sessions).  The ultimate destination is set in a couple of places in Yesod, and only one of them is controlled by the setting of *redirectToReferer* (in the YesodAuth typeclass).  In most situations we cannot avoid it being set.  The problem is that a successful login is followed by a redirection to the ultimate destination, and in an Ajax situation, this can result in being redirected, using GET, back to a resource which should be accessed with POST or PUT, for example.  To avoid this, we just clear the ultimate destination in all cases - if you want to be more selective, remember that it is stored in the session, so it survives between requests until it is cleared or used.

Finally, the call to *defaultLayout* generates the login form for the non-Ajax case.  It is essentially copied from the default login handler declared in the YesodAuth typeclass in Yesod.Auth.  I have moved few things outside, and I have used a site title set in our messages file(s) rather than the one from Yesod.Auth.Message.  This is one place you can modify the HTML generated, for example by wrapping each authentication widget in a *div* with a recognisable *id* so that you can apply CSS styles.

## Testing

It is a good idea to write some tests, and it is easy to do with Yesod.Test.  To construct an Ajax request (of the sort I have been dealing with), we need set up the parameters, and make an HTTP request which includes the correct *X-Requested-With* header.  Here is one reasonably general function for doing it, which assumes that any data is provided as a Map whose keys mirror the attributes of the JavaScript object used on the real client:

```haskell
import qualified Data.Map as Map

ajaxRequest :: StdMethod -> B.ByteString -> Map.Map Text Text -> OneSpec conn ()
ajaxRequest method url datacontent = do
    let params = mapM_ (uncurry byName) $ Map.toList datacontent
    doRequestHeaders (renderStdMethod method)
                     url
                     [("X-Requested-With", "XMLHttpRequest")]
                     params
```

*doRequestHeaders* is a very recent addition to Yesod.Test.  If you want to use it at the moment, you will need to pick up the latest version from GitHub - you can simply make a local copy of Test.hs and import that.

Yesod.Test gives us a way of getting at the *raw* response body.  Any decoding needs to be allowed for explcitly, which in our example means remembering that the body is utf-8 encoded.

We write two separate top-level specs, so that we can control the order - the ordering of the *it* specs within a single *describe* is probably not what you expect.

```
testdata = "A unicode string\x2122"
url = "/clipboard/..."   -- In a real example, get this from the server

clipSpecs1 :: Specs
clipSpecs1 = describe "The clipboard (part 1)" $ do
    it "can be set" $ do
      ajaxRequest PUT url $ Map.fromList [ ("clip", testdata) ]
      statusIs 200

clipSpecs2 :: Specs
clipSpecs2 = describe "The clipboard (part 2)" $ do
    it "can be read, producing the utf-8 encoding of what was stored" $ do
      ajaxRequest GET url Map.empty
      bodyEquals $ map (chr . fromIntegral) (B.unpack $ encodeUtf8 testdata)
      statusIs 200
```

Actually that is not quite all: for our running example we need to log in in each *it* spec.  A recipe for packaging that is the subject of [[another cookbook article|Performing Authentication during Testing]].

## Appendix: Example handler and client-side code

This simple handler, and matching JavaScript, motivates the assumptions made in the discussion above, and also just might help someone get started!

In this example, users have to be logged in, and by the time we reach the handler function, this has already been checked because of a suitable definition of *isAuthorized* in the Yesod instance in Foundation.hs.  For more on authentication and authoriation, see the [Yesod Book](http://www.yesodweb.com/book/authentication-and-authorization).

Each user has a persistent clipboard for cutting and pasting things, which for our present purposes can be taken as unicode strings.  The server simply stores the data and returns it to the client when requested, so we need read and write operations, which rather naturally map onto HTTP GET and PUT methods.  They will act on entries in a database table, defined like this in *config/models*:

```
ClipBoard
    user UserId          -- Who owns this clipboard
    data Text            -- The data
    UniqueCbUser user
```

Elsewhere in the application, a single clipboard entry is set up for each user, but for this example we can just assume that it exists.

The route is

```
/clipboard/#ClipBoardId ClipboardR GET PUT
```

and the handler code is

```haskell
module Handler.Clipboard (
    getClipboardR,
    putClipboardR
) where

import Import
import Yesod.Auth (requireAuthId)
import Control.Monad (when)

checkEntry :: ClipBoardId -> Handler ClipBoard
checkEntry cbid = do
    userid <- requireAuthId   -- Auth already checked, but we need the user
    cb <- runDB $ get404 cbid
    when (clipBoardUser cb /= userid) $
        permissionDenied "Incorrect user - did you invent the URL?"
    return cb

getClipboardR :: ClipBoardId -> Handler RepPlain
getClipboardR cbid = do
    cb <- checkEntry cbid
    return $ RepPlain $ toContent $ clipBoardData cb

putClipboardR :: ClipBoardId -> Handler ()
putClipboardR cbid = do
    _ <- checkEntry cbid
    d <- runInputPost $ ireq textField "clip"
    runDB $ update cbid [ClipBoardData =. d]
```

There are a few things to notice here:

* In the GET handler, the response is simply a utf-8 encoded string sent with type *text/plain* (*toContent* does the utf-8 encoding for Text).
* The PUT handler uses an "Input Form" (see the [Forms chapter of the Yesod Book](http://www.yesodweb.com/book/forms)).  In this case there is a single parameter to decode as Text, but if we had sent a more complex structure on the JavaScript side, there would have been further form fields here.
* Two errors can occur, both most likely caused by not following the official URL.  In a real application, the URL would have been provided by the server, so these errors should not occur unless a user types a URL by hand.

Finally, glossing over the way the client gets hold of the URL, jQuery code can send data like this:

```javascript
$.ajax ({ type: "PUT",
          url: "/clipboard/" + clipid,
          data: { clip: clipdata },
          dataType: "text",
          success: function () { alert ("It worked") },
          error:   function (jqxhr) {
              alert ("error response: " + jqxhr.responseText);
          }
       });
```

and get it back again this way:

```javascript
$.ajax ({ type: "GET",
          url: "/clipboard/" + clipid,
          dataType: "text",
          success: function (data) {
              alert ("The data was: " + data);
          },
          error:   function (jqxhr) {
              alert ("error response: " + jqxhr.responseText);
          }
       });
```
