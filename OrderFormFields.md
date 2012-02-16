# Change the order of form fields

If you have an applicative form, but you don't want the form fields to be displayed in the same order as the field of your data type, then a lambda is the easiest solution:

    data D1 = D1 { d1f1::T.Text
                 , d1f2::(Maybe T.Text)
                 , d1f3::Integer
                 } deriving (Eq, Show)
    
    form :: RenderMessage master FormMessage =>
             Maybe D1 -> Html ->
             Form sub master (FormResult D1, GWidget sub master ())    
    form d = renderDivs $ (\a b c -> D1 b a c)
      <$> aopt textField "field2" (d1f2 <$> d)
      <*> areq textField "field1" (d1f1 <$> d)
      <*> areq intField  "field3" (d1f3 <$> d)

If you need more control over the form layout than this, it's best to switch to monadic forms.