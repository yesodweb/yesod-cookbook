# Testing Yesod App

To test your app, you should first configure it with:

```
cabal configure --enable-tests
cabal install --enable-tests
```

Then you build it and run `test` command:

```
cabal build
```

```
cabal test
```