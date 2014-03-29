# Update
There's now some middleware available on hackage to help with CORS: http://hackage.haskell.org/package/wai-
cors-0.1.1

***

> [WOFF fonts](https://en.wikipedia.org/wiki/WOFF) may not be accessed from [other domains by default](https://developer.mozilla.org/en-US/docs/HTTP/Access_control_CORS).  That means that your page at `http://www.example.com/` may not access a font `http://static.example.com/font.woff`.  The solution is to add a CORS header to the WOFF font allowing it to be used from other domains.

> A simple way of doing so is by rewriting all responses whose `Content-type` is `application/font-woff` in order to include the CORS header.  You may do so using the following simple [WAI middleware](http://hackage.haskell.org/packages/archive/wai/1.4.0/doc/html/Network-Wai.html#t:Middleware):

> ```haskell
> -- | Add a permissive CORS header to WOFF files.
> --
> -- Written for wai-1.4.0 and http-types-0.8.0 but may work on
> -- many previous or next versions.
> addCORStoWOFF :: W.Middleware
> addCORStoWOFF app = fmap updateHeaders . app
>   where
>     updateHeaders (W.ResponseFile    status headers fp mpart) = W.ResponseFile    status (new headers) fp mpart
>     updateHeaders (W.ResponseBuilder status headers builder)  = W.ResponseBuilder status (new headers) builder
>     updateHeaders (W.ResponseSource  status headers src)      = W.ResponseSource  status (new headers) src
>     new headers | woff      = cors : headers
>                 | otherwise =        headers
>       where woff = lookup HT.hContentType headers == Just "application/font-woff"
>             cors = ("Access-Control-Allow-Origin", "*")
> ```