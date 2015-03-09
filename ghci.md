## Running database and handler actions from GHCI

Use `cabal repl` to start a GHCi session:

```
cabal repl
```

The `Application` module of the scaffolding provides the [`db` and `handler` functions](https://github.com/yesodweb/yesod-scaffold/blob/bb7897b67021f42aa38c7f2fedd03be81f8980c9/Application.hs#L161-L171) to run database requests and `Handler` actions from the REPL:

```haskell
ghci > import Application
ghci > db $ selectList [UserName ==. "foo"] []
```

```haskell
ghci > import Application
ghci > renderingFunction <- Application.handler $ getUrlRender
ghci > renderingFunction HomeR
"http://localhost:3000/"
```

Note that many `Handler` actions only make sense in the context of an actual HTTP request, and they may fail when from GHCi.

```haskell
ghci > handler $ notModified
*** Exception: runFakeHandler issue: InternalError "runFakeHandler: no result"
```

## Using the debugger with Yesod

Assuming you wanted to debug the following code, which you've added to `Model.hs`:

```haskell
-- * Model.hs lines 13-17
foo :: [Integer] -> [Integer]
foo xs =
    let ys = map (+1) xs
        zs = map (+2) xs
    in ys ++ zs
```

Start a GHCi session with `cabal repl`:

```
cabal repl
```

Set the module to be interpreted:

```
ghci > :add *Model # The asterisk makes it interpreted
```

Add your breakpoint:

```
ghci > :break Model 17
Breakpoint 0 activated at Model.hs:17:8-15
```

Run the `foo` function to stop at your breakpoint and evaluate intermediate variables:

```haskell
ghci > foo [1,2,3]
Stopped at Model.hs:17:8-15
_result :: [Integer] = _
ys :: [Integer] = _
zs :: [Integer] = _

ghci > ys
[2,3,4]

ghci > zs
[3,4,5]
```

You can also hit breakpoints from a URL request from your browser:

```
ghci > :add *Handler.Home
ghci > :break Handler.Home 20
ghci > Application.appMain -- Start the server
```

*Now hit http://localhost:3000 from your browser.*

```
GET /
  Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8
Stopped at Handler/Home.hs:(20,5)-(23,32)
_result :: HandlerT App IO Html = _
formEnctype :: Enctype = _
formWidget :: Widget = _
handlerName :: Text = _
submission :: Maybe (FileInfo, Text) = _

ghci > handlerName
"getHomeR"
```

### DevelMain

ghci (which you should use via `cabal repl`) can be used to inspect and run your Yesod app.

The latest scaffold provides a file [app/DevelMain.hs](https://github.com/yesodweb/yesod-scaffold/blob/postgres/app/DevelMain.hs) that you can run your Yesod app with and achieve fast reloads.
That file has instructions at the top. This page has some advance info.

## Killing threads

Restarting ghci does not kill running threads. One technique to handle this is to register all your threads so you have an easy target for shutting them down.


``` haskell
forkHandler :: (SomeException -> Handler ()) -> Handler () -> Handler ()
-- This is used to avoid accumulating threads in ghci
forkHandler exHandle handler | not development = Yesod.forkHandler exHandle handler
forkHandler exHandle handler | otherwise = do
    app <- getYesod
    let updateTids = atomicModifyIORef' (activeThreadIds app)
  
    Yesod.forkHandler exHandle $ do
        tid <- myThreadId
        liftIO $ updateTids $ \tids -> (insertSet tid tids, ()) 
        finally handler $
            liftIO $ updateTids $ \tids -> (deleteSet tid tids, ())

shutdownApp :: App -> IO ()
shutdownApp app = readIORef (activeThreadIds app) >>= mapM_ killThread
```

This needs to be integrated into DevelMain.hs.
One aspect is that you need access to the App, so you can add this function to Application.hs

``` haskell
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    conf <- getDevelConf
    (app1, site) <- makeApplicationFoundation conf
    (p, app2) <- defaultDevelApp (return conf) (const (return app1))
    return (p, site, app2)
```

Then in DevelMain.hs add shutdownApp as a finalizer

``` haskell
start done = do
    (port, site, app) <- getApplicationRepl
    forkIO (finally (runSettings (setPort port defaultSettings) app)
                    (putMVar done () >> shutdownApp site))
```