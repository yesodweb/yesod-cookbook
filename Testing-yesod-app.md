# Testing Yesod App

To test your app, you should first configure it with (use `cabal` instead of `cabal-dev` if you're using global environment):

```
cabal-dev configure --enable-tests
```

Then you build it and run `test` command:

```
cabal-dev build
```

```
cabal-dev test
```