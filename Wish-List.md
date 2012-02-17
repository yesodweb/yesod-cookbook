# Wishlist

See the [TODO list](/page/todo) for tasks that will be implemented. Below is a list of Yesod features that would be nice, but that we can continue to live without. Volunteers are welcome:

* Solution for integration testing using webkit. funcunit.js has some good ideas, but they try to couple it to steal.js and I couldn't get it to get pages from my localhost. The Ruby project ghostbuster may be the easiest approach, but I found it a little buggy. capybara-webkit demonstrates how we could bind directly to webkit. 
* Create a wai-handler-direct-fastcgi which uses the direct-fastcgi package instead of the C library. Discussion: https://github.com/snoyberg/wai-handler-fastcgi/commit/ca64674de3934ae8dd9a612487596db0cd049781
* http-conduit: add multipart form rendering. That’s possibly even a good project for a separate package.
* screen casts, maybe webinars/virtual meetups.
* Have client session cookie code not only optional for whole site, but optional per subsite. Follow up on Matt’s idea of “Yesod middleware” for providing features like sessions.
* Proxy subsite. Could be useful for cross-domain Ajax.
* Extend the current benchmark suite to run against Erlang, Nginx Perl module, and maybe that one Lua contender and Lift. Based on [this](http://steve.vinoski.net/blog/2011/05/09/erlang-web-server-benchmarking/), for Erlang, We may need to benchmark against both Yaws and mistulin. [Here is an article](http://www.mnot.net/blog/2011/05/18/http_benchmark_rules) on doing http benchmarking and [another](http://www.ostinelli.net/a-comparison-between-misultin-mochiweb-cowboy-nodejs-and-tornadoweb/)

* integrate Yesod's forms with digestive-functors forms or otherwise improve forms
* file uploading plugin configurable for different storage methods and providers (like Ruby's carrierwave + fog)
* reusable wiki sub-site. there are now a few Yesod wiki applications.
* A Yesod installer script
* use the numerals package for i18n numbers
* type-safe parameters- an extension to type-safe urls. so /foo/2?bar is directed to

~~~ {.haskell}
    getFoo :: Int -> String -> RepHtml
    getFoo id bar = do ...
~~~

## Persistent

There are a lot of potential tasks here, including plenty of relatively green field coding opportunities (implement a new backend), or even API redesign.

* projections, or sub-selects, where you only want a portion of your data fields returned. This was discussed on web-devel. The only reasonable implementation given was to stick either a default value or an undefined in the fields you don’t want.

## Deployment

* Integation with ekg for monitoring
* Faster deployment with binary diffs, perhaps using bsdiff of courgette.
* A documented technique for zero-downtime (amazon load balancer?)

## Community Applications

* An application that integrates with the Yesod development environment to automatically collect compile errors. If someone else has already offered a solution to a similar compile error it will try to match it and offer the solution.


## Development tools

* yesod console- load a ghci session with simple access to your Persistent models, printed out in a convenient (tabular?) format. Maybe we need to wait for improvements to ghci cabal integration.


## hamlet/templates

* a version of the hamlet template package that allows arbitrary haskell code in the templates. (might be easy using haskell-src-extra)
* a simple pass-through html template (easy)
