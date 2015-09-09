persistent-mongoDB has extended to support the most commonly used MongoDB operators in a type-safe way. However, you still may need to use the raw driver with Persistent at times.
Note also that persistent-mongoDB exposes functions to help with converting between a document and an entity: `toInsertFields`, `entityToFields`, `docToEntityEither`, and `docToEntityThrow`.

[Persistent's test case](https://github.com/yesodweb/persistent/blob/master/persistent-test/PersistentTest.hs#L600)

`runDB` is still the MongoDB Action monad, so just run MongoDB queries as normal.

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