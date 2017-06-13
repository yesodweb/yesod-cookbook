Subsite with typeclass contexts and type variables
==================================================

Create subsite data type and routes with typeclass contexts and type variables.

```
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, TypeFamilyDependencies, StandaloneDeriving, ViewPatterns, FlexibleInstances, FlexibleContexts #-}
module MySubsite.Route where

import Database.Persist.Class
import Yesod.Core

class (PersistEntity (MyType master), PathPiece (Key (MyType master))) => MyClass master where
    type MyType master = t | t -> master

data MySubsite master = MySubsite

mkYesodSubData "(MyClass master) => MySubsite master" [parseRoutes|
/#{Key (MyType master)} MyHandler GET
|]
```

Create subsite handlers and dispatcher.

```
{-# LANGUAGE TemplateHaskell, ViewPatterns, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, FlexibleContexts #-}
module MySubsite where

import Database.Persist.Class
import Yesod.Core
import MySubsite.Route

getMyHandler :: (MyClass master) => Key (MyType master) -> HandlerT (MySubsite master) (HandlerT master IO) Html
getMyHandler mtId = error "Implement me"

instance (MyClass master) => YesodSubDispatch (MySubsite master) (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesMySubsite)
```

