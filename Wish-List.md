# Wishlist

See the [[TODO list]] for tasks that will be implemented. Below is a list of Yesod features that would be nice, but that we can continue to live without. Volunteers are welcome:

* Solution for integration testing using webkit. funcunit.js has some good ideas, but they try to couple it to steal.js and I couldn't get it to get pages from my localhost. The Ruby project ghostbuster may be the easiest approach, but I found it a little buggy. capybara-webkit demonstrates how we could bind directly to webkit. 
* Create a wai-handler-direct-fastcgi which uses the direct-fastcgi package instead of the C library. Discussion: https://github.com/snoyberg/wai-handler-fastcgi/commit/ca64674de3934ae8dd9a612487596db0cd049781
* http-conduit: add multipart form rendering. That’s possibly even a good project for a separate package.
* screen casts, maybe webinars/virtual meetups.
* Have client session cookie code not only optional for whole site, but optional per subsite.
* Proxy subsite. Could be useful for cross-domain Ajax.
* Extend the current benchmark suite to run against Erlang, Nginx Perl module, and maybe that one Lua contender and Lift. Based on [this](http://steve.vinoski.net/blog/2011/05/09/erlang-web-server-benchmarking/), for Erlang, We may need to benchmark against both Yaws and mistulin. [Here is an article](http://www.mnot.net/blog/2011/05/18/http_benchmark_rules) on doing http benchmarking and [another](http://www.ostinelli.net/a-comparison-between-misultin-mochiweb-cowboy-nodejs-and-tornadoweb/)

* integrate Yesod's forms with [reform](http://www.happstack.com/docs/crashcourse/Reform.html) or otherwise improve forms
* file uploading plugin configurable for different storage methods and providers (like Ruby's carrierwave + fog)
* reusable wiki sub-site. there are now a few Yesod wiki applications.
* A Yesod installer script
* use the numerals package for i18n numbers
*   type-safe parameters- an extension to type-safe urls. so /foo/2?bar is directed to

    ```haskell
    getFoo :: Int -> String -> RepHtml
    getFoo id bar = do ...
    ```

* Better deployment. Keter is a good tool, but it could use some improvements across the board, and some feature enhancements too.
* Admin subsite, providing access to modifying database entries, viewing logs, etc. There are already a few attempts at this kind of thing.
* Easier client-side deployment. I've been using Fay, and I'm happy with it, but the process could certainly be smoother.

## Key/value stores and caching

There is a proposal to improve the per-request cache in ticket #268, based on a `Typeable` type-tagged key map. This type of keys could be used for typesafe storage in a modular way in more places, without polluting everything with type variables, for example:

* Generalize sessions, instead of a Map Text ByteString, make something akin to `(Typeable key, Serializable a) => Map key a`, the current `lookupSession` and `setSession` session map could then just be stored under one key. This would make it easier to make an `addMessage` that builds a list of messages to be displayed, and also for subsites or widgets to store their session things under their own key.
* Flexible general-purpose caching: a bit more complex than per-request cache because of the longer lifetime: use the API for making caches (preferred storage location: foundation type?) with a longer lifetime, for caching things like RSS feeds, updates from remote sites, that only need to be regenerated once in a while. API functions for automatically expiring cache items after a fixed amount of time, or explicitly expiring.

## Persistent

There are a lot of potential tasks here, including plenty of relatively green field coding opportunities (implement a new backend), or even API redesign.

* [uuid](http://www.postgresql.org/docs/8.3/static/datatype-uuid.html) primary key for Postgres 
* projections, or sub-selects, where you only want a portion of your data fields returned. This was discussed on web-devel. The only reasonable implementation given was to stick either a default value or an undefined in the fields you don’t want. I think Groundhog figured out a strategy.

## Deployment

* By default add gc-monitoring-wai or [StatsWeb](https://github.com/wmoss/StatsWeb) for monitoring
* Faster deployment with binary diffs, perhaps using bsdiff of courgette.
* A documented technique for zero-downtime (amazon load balancer?)

## Community Applications

* An application that integrates with the Yesod development environment to automatically collect compile errors. If someone else has already offered a solution to a similar compile error it will try to match it and offer the solution.


## Development tools

* yesod console- load a ghci session with simple access to your Persistent models, printed out in a convenient (tabular?) format. Maybe we need to wait for improvements to ghci cabal integration.


## hamlet/templates

* a simple pass-through html template (easy)
* a version of the hamlet template package that allows arbitrary haskell code in the templates. (might be easy using haskell-src-extra)

## sprites

* Make some reusable subsite or route, that takes a directory of images, generates a combined sprite image at compile time and a widget to insert them, something like `^{sprite SpriteR my_image_png}`