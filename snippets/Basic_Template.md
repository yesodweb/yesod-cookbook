# Basic Reproducible Stack template

``` haskell
#!/usr/bin/env stack
{- stack
     --resolver lts-11.4
     --install-ghc
     runghc
     --package yesod
 -}


{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

main :: IO ()
main = warp 3000 HelloWorld
```

Execution:

``` shellsession
$ stack filename.hs
```
