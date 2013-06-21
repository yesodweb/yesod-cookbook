 Lookup content will take a list of values and parse them
    with a transformer into the right values.  Then will map them
    into a JSON Object.
    To do this the final Data type must be an Instance of FromJSON and
    the transormer provided must convert (Text,Text) into a (Text,Val) type
    according to whatever Transformation rule you want


***


              
`lookupContent ::(FromJSON a , MonadHandler m) => ( (Text,Text)  -> (Text,Value) ) -> m (Result a)`
`lookupContent objTransformer = do `
`  req <- getRequest`
`  return $ fromJSON.object $ objTransformer <$> (reqGetParams req)` 

 
`data TestObj =  TestObj { testWidth :: Int`
                          `,testTitle :: Text `
                         ` ,testStep :: Int   `
                       `}`
            ` deriving (Show,Eq)`

instance FromJSON TestObj where 
    parseJSON (Object tObj) = TestObj <$> 
                          tObj .: "width" <*> 
                          tObj .: "title" <*> 
                          tObj .: "step" 

    parseJSON _ = fail "Rule: Expecting Test Object Received, Other"

instance ToJSON TestObj where 
    toJSON (TestObj {..}) = object 
                        [ 
                         "width" .= testWidth 
                         ,"step" .= testStep
                         ,"title" .= testTitle 
                         ]


-- | An example transformer on a get parameter string

exTransformObject :: (Text,Text) -> (Text, Value)
exTransformObject (t,v)
  | t == "width" = (t .= intVal v)
  | t == "title" = (t .=  textVal v)
  | t == "step"  = (t .=  intVal v)
  | otherwise = (t .= toJSON v)


-- | Helper functions to explicity do type conversion 

intVal :: Text -> Value
intVal = toJSON.intRead
         where
           intRead :: Text -> Int
           intRead = read.unpack


                     
textVal :: Text -> Value
textVal = toJSON

`