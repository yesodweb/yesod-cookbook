Normally yesod pulls in julius files at compile time.  If you make changes to your javascript and you want to test them in the yesod project, you have to recompile to make it pull the code in again.  While desirable and convenient for production use, this makes for a tedious wait for every javascript change at development time.  

Here's an example of forcing the javascript to load anew each time the page is loaded.  For this page, in the templates directory there is a playback.hamlet and playback.julius file.  When I make changes to the playback.julius, on reload the page will have the new changes, without having to recompile the yesod project.  

Relevant code is after the 'add line 1' and 'add line 2' comments.

```
playSongWs :: SongId -> WebSocketsT Handler ()
playSongWs sid = do
  app <- getYesod
  let writeChan = songLine app
  readChan <- (liftIO . atomically) $ dupTChan writeChan
  lift $ startSongThread sid
  (forever $ (liftIO . atomically) (readTChan readChan) >>= sendTextData)

getPlaySongR :: SongId -> Handler Html
getPlaySongR sid = do
  webSockets $ playSongWs sid
  mbsong <- runDB $ get sid
  -- added line 1: load the script
  let playbackscript =  $(juliusFileReload "templates/playback.julius")**
  case mbsong of
    (Just song) ->
      defaultLayout $ do
        aDomId <- newIdent
        -- added line two: add the script 
        toWidgetHead playbackscript
        setTitle "Song Playback!"
        $(widgetFile "playback")
    Nothing -> error "song not found"
```