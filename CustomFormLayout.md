# Custom form layout

Use monadic forms to get full control over the form layout. Here's a simple example. The html argument is used to add extra fields to the form, for POST forms this is a hidden nonce field to prevent cross-site request forgery.

    data D1 = D1 { d1f1::Text
                 , d1f2::(Maybe Text)
                 , d1f3::Integer
                 } deriving (Eq, Show)

    form :: RenderMessage master FormMessage =>
            Maybe D1 -> Html ->
            Form sub master (FormResult D1, GWidget sub master ())
    form d = \html -> do
      (r1, v1) <- mreq textField "field1" (d1f1 <$> d)
      (r2, v2) <- mopt textField "field2" (d1f2 <$> d)
      (r3, v3) <- mreq intField  "field3" (d1f3 <$> d)
      let vs = [v1, v2, v3]
      return (D1 <$> r1 <*> r2 <*> r3, [whamlet|
        #{html}
        $forall v <- vs
          <div>
            <div .label>#{fvLabel v}
            $maybe tt <- fvTooltip v
              <div .tooltip>#{tt}
            <div .input>^{fvInput v}
            $maybe err <- fvErrors v
              <div .error>#{err}
      |])

