So you want to keep state in yesod?

# Disclamer

Please read the disclamer on [[Keeping (in memory) state with warp]] carefully. It completely applies to keeping state with yesod, too.

# Thread-safe state

Same as described in [[Keeping (in memory) state with warp]]. While you're there, _**make sure to read the disclamer**_.

# How to inject state into yesod?

It's actually very simply: Just put it into the yesod foundation type. Your Handler can then access it using [getYesod or getYesodSub](http://hackage.haskell.org/packages/archive/yesod/0.4.1/doc/html/Yesod-Handler.html).

# Example: Counting requests

As we did with [[Keeping (in memory) state with warp]], let's count the number of requests using an IORef. For that, we only need to slightly modify the [Hello World Example](http://www.yesodweb.com/book/basics). "-- (n)" indicates a that there's something that I want to comment on below.

```haskell
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
import Data.IORef (IORef, newIORef, atomicModifyIORef)             
import Yesod

data HelloWorld = HelloWorld {
    counter :: IORef Integer -- (1)
}

mkYesod "HelloWorld" [parseRoutes|
/ CounterR GET -- (4)
|]

instance Yesod HelloWorld

incCount :: (Num a, Show a) => IORef a -> IO a
incCount counter = atomicModifyIORef counter (\c -> (c+1, c)) -- (6)

getCounterR :: Handler RepHtml
getCounterR = do 
    yesod <- getYesod -- (5)
    count <- liftIO $ incCount $ counter yesod -- (7)
    liftIO $ putStrLn $ "Sending Response " ++ show count -- (8)
    defaultLayout [whamlet|Hello World #{count}|] -- (9)

main :: IO ()
main = do
    counter <- newIORef 0 -- (2)
    warpDebug 3000 $ HelloWorld { counter = counter } -- (3)
```

So let's go through this step by step:

0. `HelloWorld` is our foundation type. We use record syntax to name the field `counter` appropriately. This makes it easier extend the foundation type further (e.g. adding a second counter that counts something else).

1. In the main function, the very first thing we do is to create a new `IORef` and store it in `counter`. We need to use `<-` because `newIoRef` returns an `IO IORef a`, but we want to access the `IORef a` inside an IO monad. This makes sense: `newIORef 0` returns different IORefs on every call, that is expressed by that return type.

2. We can then use that IORef to create a new instance of our `HelloWorld` type and give that to `warpDebug`, which runs the yesod application inside the haskell warp webserver.

3. When a browser requests `/`, it will be handled by our `getCounterR` function. 

4. The first thing `getCounterR` does is get the `HelloWorld` instance we created in `main`. As `getYesod` returns a Handler monad, we use `<-` to directly access the `HelloWorld` inside that monad.

5. We then call `incCount` on the `counter` (which is an IORef Integer) stored in `HelloWorld`. This functions increments the counter by one and returns the old value. For that, it uses `atomicModifyIORef`, which does this modification in a thread-safe manner. The function returns a tuple: The first element of the tuple is the new state, the second element of the tuple is the state returned. Here, it is the count before incrementing. Thus, for the first request this returns 0, for the second 1, for the third it returns 2, etc.

6. The result of incCount is lifted into the `Handler` monad, so that we can call `<-` on it and treat it as an Integer inside the monad.

7. We print "Sending Response $count" to the standard output

8. And use the count variable inside whamlet. That's it!

You should be able to compile & run this program using (asusming it's called yesod-counter.hs):

    ghc yesod-counter.hs && ./yesod-counter

Which should output a bunch of `Loading package` and then finally:

    Linking yesod-counter ...
    Application launched, listening on port 3000

Now when you point a browser to it, the browser will display "Hello World 0" on the first page load, "Hello World 1" on the second page load and so forth.

Meanwhile, your console should show:

    Sending Response 0
    127.0.0.1 - - [25/Nov/2012:14:21:39 +0100] "GET / HTTP/1.1" 200 - "" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_2) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.64 Safari/537.11"
    127.0.0.1 - - [25/Nov/2012:14:21:39 +0100] "GET /favicon.ico HTTP/1.1" 404 - "" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_2) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.64 Safari/537.11"
    Sending Response 1
    127.0.0.1 - - [25/Nov/2012:14:21:40 +0100] "GET / HTTP/1.1" 200 - "" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_2) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.64 Safari/537.11"
    127.0.0.1 - - [25/Nov/2012:14:21:40 +0100] "GET /favicon.ico HTTP/1.1" 404 - "" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_2) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.64 Safari/537.11"

The lines "Sending Response 0" and "Sending Response 1" are written by us, while the lines starting with an IP address are the debug output of `warpDebug`.