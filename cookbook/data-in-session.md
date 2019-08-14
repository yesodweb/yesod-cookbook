# Adding Data Type in Session 

This is a example using simple YesodWeb Template.

### Json Step

My suggestion is to use Json string to save in session.

```
data MyResponse = MyResponse {
      msg :: Text
} deriving (Show, Eq)

instance ToJSON MyResponse where
    toJSON (MyResponse x) = object [ "msg" .= x ]

data Person = Person 
    { age :: Int
    }   
    deriving (Show, Eq)

instance FromJSON Person where
    parseJSON = withObject "Person" $ \v -> Person
        <$> v .: "age"

instance ToJSON Person where
  toJSON (Person age) = object ["age" .= age]

  toEncoding (Person age) = pairs ("age" .= age )
```

### Turning Json into Text

If we try to encode (from Data.Aeson) direct (encode $ person) an error is raised because encode return a ByteText and session must be Text.
So we need to work with this. I used encodeToLazyText :: ByteText -> Data.Text.Lazy and then toStrict :: Data.Text.Lazy -> Text.

```
setSession "object"  ((toStrict $ encodeToLazyText $ person) :: Text) 
```

### Turning Text into Json

When we want to do the way back we need to use fromStrict :: Text -> Data.Text.Internal.Lazy.Text, TL.encodeUtf8 :: Data.Text.Internal.Lazy.Text -> ByteString and then decode :: ByteString -> Maybe a

```
decode $ TL.encodeUtf8 $ fromStrict objectInSession :: Maybe Person
```

### Full ApiExample
```
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.ApiExample where
import Import
import Data.Aeson (withObject, pairs, decode)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Aeson.Types as J

data MyResponse = MyResponse {
      msg :: Text
} deriving (Show, Eq)

instance ToJSON MyResponse where
    toJSON (MyResponse x) = object [ "msg" .= x ]

data Person = Person 
    { age :: Int
    }   
    deriving (Show, Eq)

instance FromJSON Person where
    parseJSON = withObject "Person" $ \v -> Person
        <$> v .: "age"

instance ToJSON Person where
  toJSON (Person age) = object ["age" .= age]

  toEncoding (Person age) = pairs ("age" .= age )

postExampleSetObjectInSessionR :: Handler Value
postExampleSetObjectInSessionR = do
    mayeObjectInSession <- lookupSession "object"
    case mayeObjectInSession of
        Nothing -> do
            let person = Person 29
            setSession "object"  ((toStrict $ encodeToLazyText $ person) :: Text)
            sendStatusJSON status201 person
        Just objectInSession ->
            case (decode $ TL.encodeUtf8 $ fromStrict objectInSession :: Maybe Person) of
                Just object -> do
                    liftIO $ print object
                    sendStatusJSON status201 object
                Nothing ->
                    sendStatusJSON status400 (MyResponse { msg = "Some error happend in decode"})
```