This page mostly focuses on detailed views of breaking changes, not necessarily covering new features. For a higher-level view, see [[Changelog]].

# Not yet released Yesod 1.2

* Removed a number of deprecated functions from Yesod.Widget. These can be replaced with `toWidget`, `toWidgetHead`, `toWidgetBody`, and `toWidgetMedia`. `toWidgetMedia` was added in 1.2 and replaces `addLuciusMedia` and `addCassiusMedia` as a means of adding CSS that only applies to specific media types.
* yesod-json has been deprecated. Its functionality has been folded into yesod-core itself.
* Felipe's newer, more efficient session code is the default. More information: https://github.com/yesodweb/yesod/issues/415#commit-ref-2009165

# 2013-02-18

* The Yesod ecosystem now supports conduit 1.0. This release of conduit is largely backwards compatible, and therefore most code will work with both conduit 0.5 and 1.0 (as is the case for Yesod itself). Little user modification should be necessary. For more information, see [the blog post](http://www.yesodweb.com/blog/2013/02/upcoming-conduit-1-0).
* wai 1.4 has been released, which adds a new field `requestBodyLength` to the `Request` datatype.
* monad-logger 0.3 is out, which no longer provides instances for `IO`. As a result, in a few places you may need to explicitly use a transformer to deal with logging. This will most commonly occur in the scaffolding with the runMigration call. The simplest thing to do is import `Control.Monad.Logger` and add `runNoLoggingT` to the beginning. For a more sophisticated approach, see [the changes to the scaffolding](https://github.com/yesodweb/yesod-scaffold/commit/578ae4068fd99c3a8ed219db75f477e44da484a4#L0R66).

# 2012-12-27

* yesod-core 1.1.7 has introduced a new means of creating session backends (clientSessionBackend2) and deprecated the old one. Going forward, the old method will be removed, which will allow us some significant performance optimizations. See: https://github.com/yesodweb/yesod/pull/418
* hamlet 1.1.3.1 now gives you an error message when a class name contains a hash, to avoid the problem of IDs accidentally being slurped up by class names. See: https://github.com/yesodweb/shakespeare/issues/75