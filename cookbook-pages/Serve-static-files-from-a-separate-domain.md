The scaffolding site has at some points had code baked in to serve static files from a separate domain name. This has sometimes led to confusion, and is therefore not always a good thing to have in the scaffolding. This Wiki page instead documents how to accomplish this.

It can be a powerful optimization can be serving static files from a separate
domain name. This allows you to use a web server optimized for static
files, more easily set expires and cache values, and avoid possibly
costly transference of cookies on static files. For more information,
please see: http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain

All of the work for this goes into the `urlRenderOverride` method of the `Yesod` typeclass. A simple implementation would be:

```haskell
urlRenderOverride master (StaticR s) =
    Just $ uncurry (joinPath master "http://static.example.com") $ renderRoute s
urlRenderOverride _ _ = Nothing
```