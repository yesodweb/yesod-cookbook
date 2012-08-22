You will need to use the raw driver with Persistent to deal with embedded fields.
Note also that persistent-mongoDB exposes `toInsertFields` and `entityToFields` are available to help with embedded documents.

``` haskell
import Database.MongoDB((=:))
import Database.MongoDB (Document, Action, findOne)
import qualified Database.MongoDB as MongoDB

rawOne :: Trans.MonadIO m => MongoDB.Collection -> MongoDB.Selector -> Action m (Maybe Document)
rawOne collection q = findOne (MongoDB.select q collection)

theCollection :: Trans.MonadIO m
theCollection = rawOne $ "collection-name"

getTheR theId = do
  Just doc <- runDB $ theCollection ["_id" =: theId]
```
