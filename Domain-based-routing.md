```haskell
-- This example shows how you can perform routing based on the domain name.
-- Normal stuff up here...
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    TypeFamilies, OverloadedStrings #-}
import Yesod
import Control.Arrow ((***))
import Network.Wai.Handler.Warp (run)
import Network.Wai
import Network.HTTP.Types
import Data.Monoid (mappend)
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

data Subdomain = Subdomain

-- Here's the trick: the subdomain will be encoded as the first piece in the
-- path. This gives us lots of flexibility; we can match explicitly against
-- some domains (e.g., www.mydomain.com), or use wildcards (e.g. &lt;username&gt;).
-- This is a purposely simple example to be run on a localhost system without
-- playing with your hosts file.
mkYesod "Subdomain" [parseRoutes|
/localhost LocalhostR GET
/127.0.0.1 OnetwosevenR GET
|]

instance Yesod Subdomain where
    -- approot is entirely ignored for this application
    approot _ = ""

    -- And here's the first bit of magic. joinPath is what turns our list of
    -- path pieces into a URL. So we grab the first piece, treat is as a domain
    -- name, and otherwise do normal processing (encoding query string
    -- parameters, etc).
    joinPath _ _ (domain:pieces') qs' =
    copyByteString "http://"
    `mappend` fromText domain
    -- Insert the port in as well. Obviously if you have a port besides
    -- 3000 you'd want to change this. A more robust approach would be to
    -- define a variable containing the port, and use it both here and in
    -- main.
    `mappend` copyByteString ":3000"
    `mappend` encodePath pieces qs
      where
    pieces = if null pieces' then [""] else pieces'
    qs = map (TE.encodeUtf8 *** go) qs'
    go "" = Nothing
    go x = Just $ TE.encodeUtf8 x

    -- An empty path list no longer makes sense (i.e., you can't have a URL
    -- without a domain).
    joinPath _ _ [] _ = error "joinPath with null list"

-- The second piece of magic: a middleware that automatically prepends the
-- domain name to the list of path pieces. It also strips off the port (if
-- present), which means that our route definitions do not need to change for
-- port number changes.
--
-- In theory, if you want to, you could leave the port number on and route
-- based on that as well.
prependSubdomain :: Middleware
prependSubdomain app req =
    app req { pathInfo = domain : pathInfo req }
  where
    domain = T.takeWhile (/= ':')
       $ maybe "localhost" (TE.decodeUtf8With TEE.lenientDecode)
       $ lookup "host"
       $ requestHeaders req

-- Just some standard handler functions. Notice how easy it is to link to the
-- different domains.
getOnetwosevenR = defaultLayout [whamlet|
&lt;h1&gt;127.0.0.1
&lt;p&gt;
    &lt;a href=@{LocalhostR}&gt;localhost
|]

getLocalhostR = defaultLayout [whamlet|
&lt;h1&gt;localhost
&lt;p&gt;
    &lt;a href=@{OnetwosevenR}&gt;127.0.0.1
|]

main :: IO ()
main = do
    app &lt;- toWaiApp Subdomain
    -- Finally, just make sure to use the middleware.
    run 3000 $ prependSubdomain app
```