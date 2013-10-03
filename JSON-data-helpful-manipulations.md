Some Functions for JSON in Yesod.

# some functions 1

##Disclaimer: 
I use these functions inside the yesod scaffolding and am just trying to bring things out that were useful.  This isn't meant to be a compile-able example.

```haskell
{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             MultiParamTypeClasses  #-}

import qualified Data.HashMap.Strict as H
import Data.Aeson 
import Data.Text (pack)
import Data.List ()

data FullPart = FullPart 
    {  
        formPartNum :: Text
       ,formManNum  :: Text

    }

-- Putting this in Model.hs will surpress orphan instance problems if using scaffolding
instance ToJSON FullPart where
    toJSON (FullPart 
            {..}) = object
               [ 
                "formPartNumber"    .=   formPartNum
               ,"formManNum"        .=   formManNum       

               ]

appendPair:: Object -> Text-> Value -> Object
appendPair obj label val = H.insert label val obj 
``` 
So if you have a single value like a cost, coming from somewhere else, you can append it to this JSON object.

# how to render json object within hamlet template

this toJSONText function is useful when having a JSON Value that you want to render in hamlet template:

```haskell
import           Data.Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Text.Encoding   (decodeUtf8)
import           Text.Blaze.Html      (Html, preEscapedToHtml)
 
lazyToStrictBS :: LBS.ByteString -> BS.ByteString
lazyToStrictBS x = BS.concat $ LBS.toChunks x
 
toJSONText :: Value -> Html
toJSONText v = preEscapedToHtml $ decodeUtf8 $ lazyToStrictBS $ encode $ v
``` 