So you want to keep state in your web server?

# Disclamer

*Before learning on how to keep state in memory warp or yesod, please be aware of the consequences:*

* Warp will span as many OS threads as it deems appropriate. Thus, you need to write thread-safe code if you don't want multi-threading bugs.
* As some form of protection against multi-threading is required, the code will always be slower than if it was not stateful.
* Two consecutive HTTP requests that depend on state might return different responses. Be aware of that and set caching headers appropriately.
* If you web server crashes (whether the web servier itself, the underlying OS or the hardware) and the only place you stored the state was in memory, it will be lost forever.
* If you introduce in-memory state to your web server, it will be much harder to scale horizontally: A second instance of the web server will not share the state. Be aware of that. Of course, two instances could communicate somehow to synchronize their state, but that adds new problems. Distributed systems are hard.

The problems described above apply to all multi-threaded stateful web servers, independent of the underlying technology (C++ / Java / go / ...). If you use a single-threaded stateful web server (like node.js), you won't need to deal with the multi-threadedness, of course. But haskell's lightweight thread and smart solutions to dealing with multi-threaded state make that the smaller part of the problem.

# Thread-safe state

Haskell provides us with multiple options to manipulate state in a thread-safe way:

* [IORef](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-IORef.html): Those should be used if the state is rather small and updates are quick. 
* [MVars](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent-MVar.html): Basically a very good mutex (or lock) to store state in, but it carries all the problems of mutexes: They are not composable and prone to deadlocking.
* [TVars](http://www.haskell.org/ghc/docs/latest/html/libraries/base/GHC-Conc.html#g:3): State modified by Transactions. The solution to use for more complex state. A transaction can detect that it conflicted with a transaction that was carried out in parallel and will retry. See the [Haskell wiki](http://www.haskell.org/haskellwiki/STM) for a more details.

# How to inject the state into warp?

It's actually rather easy: When you run the warp server, you need to provide it with a [wai Application](http://hackage.haskell.org/packages/archive/wai/1.3.0.1/doc/html/Network-Wai.html). We simply need to add that state to the application first.

Then, any request that is processed by the application can access the state in a thread-safe way.

# Example: Counting requests

Take a look at this code listing. "-- (n)" indicates that you can find an explanation below

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai (responseLBS, Request, Response)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Control.Monad.Trans (liftIO, lift)
import Data.IORef (IORef, newIORef, atomicModifyIORef)
import Data.Conduit (ResourceT)
import Data.ByteString.Lazy as B (concat, ByteString, append)
import Data.ByteString.Lazy.UTF8 (fromString)

application :: (Num a, Show a) => IORef a -> Request -> ResourceT IO Response
application counter request = do  -- (3)
  count <- lift $ incCount counter  -- (5)
  liftIO $ printCount count -- (6)
  let responseText = makeResponseText count -- (7)
  return $ responseLBS status200 [("Content-type", "text/html")] $ responseText -- (8)

makeResponseText :: (Show a) => a -> B.ByteString
makeResponseText s = "<h1>Hello World " `append` (toByteString s) `append` "</h1>\n"

toByteString :: (Show a) => a -> B.ByteString
toByteString s = fromString $ show s

printCount :: (Show a) => a -> IO ()
printCount count = do 
  putStrLn $ "Sending Response " ++ show count

incCount :: (Num a, Show a) => IORef a -> IO a
incCount counter = atomicModifyIORef counter (\c -> (c+1, c)) -- (4)

main = do 
  putStrLn $ "Listening on port " ++ show 3000
  counter <- newIORef 0 -- (1)
  run 3000 $ application counter -- (2)
```
So what is happening here?

1. We are creating a new `IORef` called `counter` that contains the Integer 0

2. We are then running the application and *curry* that counter into it. The function that results from the currying has exactly the type required by `run'.

3. Application is called on every request. As we curried the counter into it, it have be the same (mutable) `IORef` for every request.

4. We first call `incCount` to return the current count and increment the counter by one: `incCount` uses `atomicModifyIORef` to modify the `IORef` in a thread-safe way. The function passed to `atomicModifyIORef` is very simple: It returns a tuple, as expected by `atomicModifyIORef`: The first element of the tuple is the new state, the second element of the tuple is the state returned. Here, it is the count before incrementing. Thus, for the first request this returns 0, for the second 1, for the third it returns 2, etc.

5. `incCount` returns an `IORef Integer`, but we want to treat it just like an Integer. Thus, we need to use `<-` to treat it like an `Integer` inside the monad. In order to use `<-`, we first need to lift it to `ResourceT IO Integer`.

6. Now, we can call `printCount`, which would accept an `Integer` but not an `IO Integer`. The `printCount` function is very straightforward. However, notice that we need to lift his too, so that it is properly used in the monad. If we wrote `let _ = printCount count', the compiler would optimize it away.

7. This is really straightforward: Let's make a bytestring our of the result of `Show count` and surround it with some HTML

8. This should also be straighforward: We write the generated text into the response. (Note that you can easily combine 7. and 8. into one line)

You should be able to compile & run this program using (assuming it's called warp-counter.hs):

    ghc warp-counter.hs && ./warp-counter.hs

This should display:

    [1 of 1] Compiling Main             ( warp-counter.hs, warp-counter.o )
    Linking warp-counter ...
    Listening on port 3000

Now if you point a web browser to http://localhost:3000/ that browser should display "Hello World 0" and the command line should say

    Sending Response 0

Every page reload should increase the counter by one.

(On some browsers, loading the page increments the counter by two. This is because the browser makes two HTTP requests: One for `http://localhost:3000/` and one for `http://localhost:3000/favicon.ico`. As an exercise, try to modify the program so that it prints the request path in `printCount`. Hint: Use [rawPathInfo](http://hackage.haskell.org/packages/archive/wai/1.3.0.1/doc/html/Network-Wai.html))