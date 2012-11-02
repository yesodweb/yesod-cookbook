The following is some code to generate string based enums, like countries, sectors and currencies, using Template Haskell.

The aim is to have a simple way to declare the enumeration constructors and values, and have Template Haskell generate the data type, appropriate constructors, Show and Read instances.

A usage example:

``` haskell
mkEnum "Sector" [enum|
  Accountancy AC
  Computing CO
  Medical ME
|]
```

Yesod can then generate a field for use with Persistent if you subsequently include the code `derivePersistField "Sector"`.

The Haskell code to implement `mkEnum` should be placed in a separate module, for inclusion within the module that will use the Template Haskell. This is to satisfy compilation order constraints related to Template Haskell. I referenced the TH module in the `other-modules` section of my cabal file.

Some boiler plate inclusions:

```haskell
{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Model.Enum.TH where

import Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Data.Map as Map
import Control.Arrow ((&&&))
```

Source code wise, first up is a `QuasiQuoter`. This parses the constructor / value lines, and returns the parsed structure appropriate for consumption by the application code facing `mkEnum` function.

``` haskell
enum :: QuasiQuoter
enum = QuasiQuoter { quoteExp = qe }
  where
    pair (k:v:[]) = (k, v)
    pair _        = error "Please use constructor value pair entries for enums."
    qe s = do
      let lists = filter (not . null) $ map words $ lines s
          pairs = map pair lists
      [|pairs|]
```

And finally there is code to generate the data type, the Show instance, and the Read instance. The Read instance uses more comprehensive use of quasiqouting, to make constructing the AST simpler.

``` haskell
genData :: Name -> [Name] -> DecQ
genData name keys = dataD (cxt []) name [] cons [''Eq, ''Enum, ''Bounded]
  where cons = map (\n -> normalC n []) keys

genShow :: Name -> [(Name, String)] -> DecQ
genShow name pairs =
  instanceD (cxt [])
    (appT (conT ''Show) (conT name))
    [funD (mkName "show") $ map genClause pairs]
  where
    genClause (k, v) = clause [(conP k [])] (normalB [|v|]) []

mkEnum :: String -> [(String, String)] -> Q [Dec]
mkEnum name' pairs' =
  do
    ddec <- genData name (map fst pairs)
    sdec <- genShow name pairs
    rdec <- [d|instance Read $(conT name) where
                 readsPrec _ value =
                   case Map.lookup value m of
                     Just val -> [(val, [])]
                     Nothing  -> []
                   where
                     m = Map.fromList $ map (show &&& id) [minBound..maxBound]|]
    return $ ddec : sdec : rdec
  where name  = mkName name'
        pairs = map (\(k, v) -> (mkName k, v)) pairs'
```

Beyond this, in `Foundation.hs`, I then place the following, to configure i18n messages for the enum:

```haskell
mkMessageFor "App" "Sector" "messages/sector" "en"
```