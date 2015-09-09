# Require input for a Maybe field

If you have a data with a Maybe field, but you still require that the user enters a value, you can make it a required field like this:

```haskell
data D1 = D1 { d1f1::T.Text
             , d1f2::(Maybe T.Text)
             , d1f3::Integer
             } deriving (Eq, Show)

form :: RenderMessage master FormMessage =>
         Maybe D1 -> Html ->
         Form sub master (FormResult D1, GWidget sub master ())
form d = renderDivs $ D1
  <$> areq textField            "field1" (d1f1 <$> d)
  <*> fmap Just (areq textField "field2" (d1f2 =<< d))
  <*> areq intField             "field3" (d1f3 <$> d)
```

alternatively, use this function:

```haskell
areqMaybe field fs mdef = fmap Just (areq field fs $ join mdef)

form2 :: RenderMessage master FormMessage =>
         Maybe D1 -> Html ->
         Form sub master (FormResult D1, GWidget sub master ())
form2 d = renderDivs $ D1
  <$> areq      textField "field1" (d1f1 <$> d)
  <*> areqMaybe textField "field2" (d1f2 <$> d)
  <*> areq      intField  "field3" (d1f3 <$> d)
```