Some Functions for JSON in Yesod.

#Disclaimer:  I use these functions inside the yesod scaffolding and am just trying to bring things out that were useful.  This isn't meant to be a compile-able example.

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