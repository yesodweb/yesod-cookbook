You can modify the behavior of `widgetFile` in the scaffolding by changing the `widgetFileSettings` value in `Settings.hs`. As an example:

```haskell
widgetFileSettings = def { wfsLanguages = \hset -> defaultTemplateLanguages hset ++
    [ TemplateLanguage True  "coffee"  Text.Coffee.coffeeFile   Text.Coffee.coffeeFileReload
    ] }