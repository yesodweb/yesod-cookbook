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