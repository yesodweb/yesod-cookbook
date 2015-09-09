There have been some minor changes to Yesod since the publication of the book. This page will hopefully contain a full list:

*    To ease installation, we now have a `yesod-platform` package. Instead of running `cabal install yesod`, you can run `cabal install yesod-platform`.

*    Hamlet now requires that all tags have a closing right bracket. In other words:

         <good>
         <bad

*    The function `yepnopeJs` has been generalized to `jsLoader`. Similarly, `encryptKey` and `clientSessionDuration` have been replaced with `makeSessionBackend`.

*   Persistent no longer automatically derives `Show`, `Read`, and `Eq` instances. You must manually add them in. For example:

        Person
            name Text
            age Int
            deriving Show -- this is now required