Lenses are really cool
Scaffolded Persistent Entities are really cool

The std template haskell for each doesn't play nice...

The below code will generate your lenses like:
``` haskell
Foo 
  Field1 Int 

persistMakeClassy ''Foo

-- |your lens will be 

lensFooField1 
```
## Lens generators for your existing Persist Entities

I love all the types that Persistent generates for me, but now I want lenses too
```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# Language TemplateHaskell #-}

module Persist.Scaffold.Lens where


import Control.Lens
import Control.Lens.Lens
import Control.Lens.Prism
import Control.Lens.TH
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Set as Set
import qualified Data.Foldable as F
import ContentCfgTypes
import Data.Char (toUpper)



-- |  Lens naming section ================================

mLowerName :: String -> Maybe String
mLowerName (c:cs) = Just ('l':'e':'n':'s':(toUpper c):cs)
mLowerName _ = Nothing


-- | Default 'LensRules'.
persistDefaultRules :: LensRules
persistDefaultRules = LensRules mLowerName fld (const Nothing) $
    Set.fromList [SingletonIso, SingletonAndField, CreateClass, CreateInstance, BuildTraversals, GenerateSignatures]
  where
    fld cs = mLowerName cs


-- | Rules for making lenses and traversals that precompose another 'Lens'. that won't interfere with Yesod Scaffold
persistClassyRules :: LensRules
persistClassyRules = persistDefaultRules
  & lensIso .~ const Nothing
  & handleSingletons .~ False
  & lensClass .~ classy
  & classRequired .~ True
  & partialLenses .~ False
  & buildTraversals .~ True
  where
    classy :: String -> Maybe (String, String)
    classy n@(a:as) = Just ("Has" ++ n, toLower a:as)
    classy _ = Nothing

persistMakeClassy :: Name -> Q [Dec]
persistMakeClassy = makeLensesWith persistClassyRules



```