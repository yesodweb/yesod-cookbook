To serve javascript with interpolated values we need to create a couple of instances of two Yesod classes, HasReps and ToContent.


Here is how to do so:

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