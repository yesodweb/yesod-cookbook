``` haskell
rawOne :: Trans.MonadIO m => MongoDB.Collection -> MongoDB.Selector -> Action m (Maybe Document)
rawOne collection q = findOne (MongoDB.select q collection)

theCollection :: Trans.MonadIO m
theCollection = rawOne $ u"food

getTheR theId = do
  Just doc <- runDB $ theCollection ["_id" =: theId]
```