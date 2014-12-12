If you copy the [“Hello world” subsite from the book](http://www.yesodweb.com/book/creating-a-subsite), you should be able to run it using `runghc` (if not, something else is wrong—check the versions listed at the bottom of this page). 

However, as soon as you insert a link inside the subsite, everything goes terribly awry. For instance, if you replace the definition of `getSubRootR` with:

```haskell
getSubRootR = defaultLayout [whamlet|<a href=@{SubRootR}>Link to self|]
```

You are liable to get something like: 

    simple.hs:16:38:
        Could not deduce (master ~ HelloSub)
        from the context (Yesod master)
          bound by the type signature for
                     getSubRootR :: Yesod master => GHandler HelloSub master RepHtml
          at simple.hs:16:1-71
          `master' is a rigid type variable bound by
                   the type signature for
                     getSubRootR :: Yesod master => GHandler HelloSub master RepHtml
                   at simple.hs:16:1
        Expected type: Route master
          Actual type: Route HelloSub
        In the first argument of `\ u_a3g6
                                    -> urender_a3g5 u_a3g6 []', namely
          `SubRootR'
        In the first argument of `toHtml', namely
          `\ u_a3g6 -> urender_a3g5 u_a3g6 [] SubRootR'

This is because `SubRootR` is of type `Route HelloSub`, but Yesod is expecting the routes to be the same type as `master`. One way to fix this problem is to use [`getRouteToMaster`](http://hackage.haskell.org/packages/archive/yesod-core/1.0.1.2/doc/html/Yesod-Handler.html#v:getRouteToMaster) (edit: renamed [`getRouteToParent`](http://www.yesodweb.com/blog/2013/03/big-subsite-rewrite) after Yesod 1.2) to transform `SubRootR` into type `Route master`: 

```haskell
getSubRootR = do
  toMaster <- getRouteToMaster
  defaultLayout [whamlet|<a href=@{toMaster SubRootR}>Link to self|]
```

---

Based on [a conversation with Michael Snoyman](https://groups.google.com/d/msg/yesodweb/l54kYHQJhKw/6DDYC6XDIg8J)

---

This material was tested with GHC 7.4.1 and yesod-platform 1.0.4.2. 