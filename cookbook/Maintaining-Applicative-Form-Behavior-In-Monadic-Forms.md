# Maintaining Applicative Form Behavior In Monadic Forms

Applicative forms have a number of useful features that you lose when 
switching to Monadic forms, such as generating labels, displaying errors
beside the input field, etc, any of which you would then have to re-implement
yourself in the monadic context. 

However, you can separate the overall form into individual applicative forms,
one per field, and then embed them within a monadic form, you can maintain 
the individual behavior.

Here is an example: 

```haskell
joinForm :: Html -> MForm Handler (FormResult Join, Widget)
joinForm extra = do
  let
    mustAgreeField :: Text -> Field Handler Bool

    mustAgreeField errorMsg = checkBool id errorMsg checkBoxField
    accept1Def = areq (mustAgreeField
                         "You must specify that you accept Section 1")
                    "Accept Section 1" Nothing
    accept2Def = areq (mustAgreeField
                         "You must specify that you accept Section 2")
                    "Accept Section 2" Nothing
    fullNameDef = areq
                    (checkBool
                      ((>0) . length)
                      ("You must include your full, legal name" :: Text)
                      textField)
                    "Full Name"
                    Nothing

  (accept1, accept1Widget) <- renderDivs accept1Def mempty
  (accept2, accept2Widget) <- renderDivs accept2Def mempty

  (fullName, fullNameWidget) <- renderDivs fullNameDef mempty

  let
    widget =
      [whamlet|
              ^{extra}
              <p.my-4> this is a sentence
              ^{accept1Widget}
              <p.my-4> a sep
              ^{accept2Widget}
              <p.my-4> another sep
              ^{fullNameWidget}
              |]

    joinRes = Join <$> accept1 <*> accept2 <*> fullName

  return (joinRes, widget)

data Join = Join
  { joinSignedSection1 :: Bool
  , joinSignedSection2 :: Bool
  , joinFullName :: Text
  }
  deriving Show

```

- each applicative field is converted to the monadic context via `renderDivs`
- `mempty` is passed to `renderDivs` instead of the usual `extra`, and `extra` 
  is embedded in the parent widget. Otherwise, CSRF will be included multiple 
  times, and this will not work 
