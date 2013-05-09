Update: Much more robust and easier to handle in Javascript as described in http://stackoverflow.com/questions/16173474/extjs-4-form-submit-without-sucesstrue-in-answer



ExtJS requires that the response have the attribute "success" set to "true" if the request is successful (see http://docs.extjs.com/extjs/4.2.0/#!/api/Ext.form.action.Submit ). Yesod.Auth.HashDB does not add this attribute, so as a workaround the proxy handler below forwards the request to HashDB login handler and then sends back an appropriate response.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Handler.LoginProxy where

import Import
import Data.Aeson (encode)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Conduit as C
import qualified Network.Wai as W
import Data.Conduit.Lazy (lazyConsume)
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder.ByteString (fromByteString)

postLoginProxyR :: Handler Value
postLoginProxyR = do
    yReq <- getRequest
    let req = reqWaiRequest yReq
    bss <- liftIO $ runResourceT $ lazyConsume $ W.requestBody req
    let body = L.fromChunks bss
    d <- liftIO $  do
        req' <- C.parseUrl "http://localhost:3000/auth/page/hashdb/login"
        liftIO $ C.withManager $ \man -> C.httpLbs req' {
            C.method = "POST",
            C.requestHeaders = W.requestHeaders req,
            C.checkStatus = \_ _ _ -> Nothing,
            C.requestBody = C.RequestBodyLBS body
            } man
    let rstatus = C.responseStatus d
    let success = (if rstatus == status200 then "true" else "false") :: Text
    let result = B.concat $ L.toChunks $ encode $ object [ "success" .= success ]
    sendWaiResponse $ W.ResponseBuilder
                     (C.responseStatus d)
                     (C.responseHeaders d)
                     (fromByteString result)
```