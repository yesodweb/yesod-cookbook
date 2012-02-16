# Compile Coffeescript to Javascript

This lets you capture just type-safe urls but not any changing variables and compile to javascript just once. Just use the coffeescript watcher if you don't need any interpolation.

    defaultLayout widget = do
      y <- getYesod
      mroute <- liftIO $ readIORef $ staticRenders y
      coffeeUrl <- case mroute of
          Just r -> return r
          Nothing -> do
            renderUrl <- getUrlRenderParams
            c <- liftIO $ renderCoffee renderUrl $(Settings.coffeeFile "search")
            Just (Right (u, p)) <- addStaticContent "js" "text/javascript; charset=utf-8"
                                      $ encodeUtf8 c
            let r = renderUrl u p 
            liftIO $ writeIORef (staticRenders y) (Just r)
            return r 
