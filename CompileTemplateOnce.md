# Compile a template once

Mostly useful if you are only inserting URLs and no variables. Any variables will only be rendered once. This example is using coffeescript.

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
