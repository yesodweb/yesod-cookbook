While using derivePersistField can provide an easy method for storing Haskell simple/sum types there are cases were you will 
to need to use native DB types for efficiency or to use type specific functions which can be helpful in advanced queries.
This cookbook provides examples for two Postgresql types JSONB, INTERVAL, and UUID

To support native encoding to class instances need to be created.
* `PersistFieldSql` class specifies which Posgresql type should be created for the database migration.
* `PersistField` class specifies how to martial the custom type into and out of the database.
`PersisteFieldSql` contains only one method `sqlType` which returns a value of type `SqlType` For both JSONB and INTERVAL we 
will be using the `SqlOther` constructor, because no other defined type is suitable.
`PersistField` class takes to methods :
* `toPersistValue` which takes your defined type and converts it to a PersistValue value, again because there is no existing 
suitable type we will be using `PersistDbSpecific` to encode both JSONB and INTERVAL.
* `fromPersistValue` takes `PersistValue` and converts it to you custom type wrapped in an `Either` container. The `Left` side value of
 the either is an error message as a `Text` value.

For parsing 
```haskell
import qualified Data.Aeson as A
import Data.Aeson.Parser (value)
import qualified Data.Attoparsec.ByteString.Char8 as AP
```

### JSONB
[JSONB](http://www.postgresql.org/docs/current/static/datatype-json.html) is a binary encoded type for storing JSON files. The
value is specified as a normal JSON type. In addition to Arrays and JSON objects, JSONB supports scalars and null.

Using the existing Aeson parser simplifies the task of marshling the data to and from JSONB. To fully support JSONB including
scalars we need to use the `value` parser rather than `decode`. And convert eitherResult which outputs `Either String` to 
`Either Text` as declared by `fromPersistValue`.  It to some trial and error to determine that Persist initially converted 
the JSONB value to a PersistByteString, so that is all we need to support.

```haskell
data JsonB = JsonB A.Value
    deriving (Show, Eq)

instance PersistField JsonB where
  toPersistValue (JsonB t) = PersistDbSpecific $ toStrict $ A.encode t
  fromPersistValue (PersistByteString s) = either (Left . pack . ("Could not convert Json " ++)) (Right . JsonB) $ AP.eitherResult $ AP.parse value $  s
  fromPersistValue a = Left $ pack ("JsonB conversion failed for value " ++ show a)

instance PersistFieldSql JsonB where
  sqlType _ = SqlOther "jsonb"
```
### INTERVAL
[INTERVAL](http://www.postgresql.org/docs/current/static/datatype-datetime.html) is used for specifing time intervals.
The Postgresql specification allows interval specifications which include units format, such as
'2 hours' or '34.5 minutes', but without any units the units as seconds.
We will be taking advantage of the last one for encoding. As output Postgresql uses the following 
format similar to time '85:20:23.33' which represents 85 hours 20 minutes and 23.33 seconds.

In this case Persistent converts the INTERVAL string to a PersistDbSpecific. We then write a simple parser which takes
the interval string as described above and converts it to a Double in seconds. attoparsec has a quirk which allows
for return of a partial parse so unless we feed an extra character to end the consuming of double the parser 
will fail in eitherResult.  In this case we feed in the null character "\0"

```haskell
parseInterval :: AP.Parser Interval
parseInterval = do
  hours <- AP.decimal
  _ <- AP.char ':'
  minutes <- AP.decimal
  _ <- AP.char ':'
  seconds <- AP.double
  return $ Interval $ (fromIntegral (hours*60+minutes)*60)+(seconds)

data Interval = Interval Double
    deriving (Show, Read, Eq)

instance PersistField Interval where
  toPersistValue (Interval r) = PersistDbSpecific $ fromString $ show r
  fromPersistValue (PersistDbSpecific s) =  either (Left . pack . ("Could not convert to Interval " ++)) (Right) $ AP.eitherResult $ AP.parse parseInterval (s ++ "\0")
  fromPersistValue a = Left $ pack ("Interval conversion failed for value " ++ show a)

instance PersistFieldSql Interval where
  sqlType _ = SqlOther "interval"
```
### UUID
[UUID](http://www.postgresql.org/docs/9.5/static/datatype-uuid.html) for storing Universally Unique Identifiers.
For the `toPersistValue` we only need to convert the UUID to a `Text` value and PersistValue the value using constructor `PersistText`. The library uuid has functions convert UUID to and from `String` and to and from `Text`. For `fromPersistValue` the UUID is embedded in a `PersistDbSpecific`value. We first must convert the `ByteString` to a normal `String` and then convert the string to a UUID. 
```haskell
import qualified Data.ByteString.UTF8 as BSU
import Data.UUID.Types (UUID(..), fromText, toText)
import qualified  Data.UUID.Types as UU -- for fromString

instance PersistField UUID where
  toPersistValue = `PersistText` . toText
  fromPersistValue raw@(PersistText s) =  maybe (Left ("Could not convert to UUID " ++ s)) (Right) $ fromText s
  fromPersistValue raw@(PersistDbSpecific t) = maybe (Left $ pack ("Could not convert to UUID " ++ show raw)) (Right ) $ UU.fromString $ BSU.toString t
  fromPersistValue a = Left $ pack ("UUID conversion failed for value " ++ show a)

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"
```
If you want to replace the primary id with a UUID, add the following to your model
  ID UUID default=uuid_generate_v1()

To use UUID automatic generation on postgres enable [uuid-ossp](http://www.postgresql.org/docs/current/static/uuid-ossp.html)
 in postgresql use:
'''
enable_extension 'uuid-ossp'
'''
alternatively if you only need UUID V4, you can use the function gen_random_uuid in the pgcrypto module.
