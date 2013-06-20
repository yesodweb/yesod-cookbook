To serve javascript with interpolated values we need to create a couple of instances of two Yesod classes, HasReps and ToContent.


Here is how to do so:

```haskell
import Yesod.Routes.Class (Route)

newtype RepJavascript = RepJavascript Content                                   
instance ToTypedContent RepJavascript where                                     
  toTypedContent (RepJavascript c) = TypedContent typeJavascript c              
instance HasContentType RepJavascript where                                     
  getContentType _ = typeJavascript                                             
deriving instance ToContent RepJavascript                                       
instance ToContent Javascript where                                             
  toContent = toContent . renderJavascript                                      

juliusToRepJavascript :: (ToContent a, MonadHandler m) => 
                     => ((Route (HandlerSite m) -> [(Text,Text)] -> Text) -> a) -> m TypedContent                                                                                
juliusToRepJavascript j =  do                                                   
  render <- getUrlRenderParams                                                  
  return $ toTypedContent $ RepJavascript (toContent $ j render)                
```

Then you can use it like this:

```haskell                                                                                
getJavascript :: Handler TypedContent                      
getJavascript = do                                            
  juliusToRepJavascript $(juliusFile "templates/myfile.julius")
```

for version prior to Yesod 1.2:

```haskell
newtype RepJavascript = RepJavascript Content

instance HasReps RepJavascript where
    chooseRep (RepJavascript c) _ = return (typeJavascript, c)
instance ToContent Javascript where
    toContent = toContent . renderJavascript

juliusToContent :: Monad mo
            => JavascriptUrl (Route master) -> GGHandler sub master mo Content
juliusToContent j = do
    render <- getUrlRenderParams
    return $ toContent $ j render

juliusToRepJavascript :: Monad mo
            => JavascriptUrl (Route master) -> GGHandler sub master mo RepJavascript
juliusToRepJavascript = liftM RepJavascript . juliusToContent
```

You can use this in your handler like so:
    
```haskell
getHandlerR = do
    juliusToRepJavascript $(juliusFile "myfile")
```