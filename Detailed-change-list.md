This page mostly focuses on detailed views of breaking changes, not necessarily covering new features. For a higher-level view, see [[Changelog]].

# Not yet released Yesod 1.2

* json requests for authenticated/authorized routes will return a proper response code (401 or 403) instead of a redirection response
* Removed a number of deprecated functions from Yesod.Widget. These can be replaced with `toWidget`, `toWidgetHead`, `toWidgetBody`, and `toWidgetMedia`. `toWidgetMedia` was added in 1.2 and replaces `addLuciusMedia` and `addCassiusMedia` as a means of adding CSS that only applies to specific media types.
* yesod-json has been deprecated. Its functionality has been folded into yesod-core itself.
* Felipe's newer, more efficient session code is the default. More information: https://github.com/yesodweb/yesod/issues/415#commit-ref-2009165
* Everything in Yesod.Handler now lives in a typeclass. All functions work in the master site by default, with a few functions provided to deal with subsites. The most important change here was getCurrentRoute, which now has much more sane behavior.
* Overhaul to the content/representation breakdown. RepHtml et al are deprecated. ChooseRep is gone. Now we have Content, TypedContent, and HasContentType. NOTE: This needs more explanation, I'll write a blog post about this and the YesodRequest/YesodResponse switch.
* yesod-default has been deprecated. Its functionality has been folded into the yesod package.
* GHandler and GWidget are replaced entirely with HandlerT and WidgetT. We no longer have a modified `lift` function, since everything is a standard transformer. You need not (and in fact, can not) lift Handler function into a Widget.
    * If you need to lift ResourceT actions into a Handler or Widget, use liftResourceT.
    * If you need to lift HandlerT actions into a Widget, use handlerToWidget. (Depending on how we go forward with the Handler typeclasses, we may provide liftHandler.)
* Subsite creation has been overhauled entirely. Need a blog post on this.
* Changed some Yesod typeclass functions: getLogger is now makeLogger. messageLogger, logLevel, gzipSettings have been removed.
* toWaiApp no longer applies any middlewares.
* MForm takes three arguments instead of 4 (no subsite). A typical `Form` type synonym in Foundation.hs would be: type Form x = Html -> MForm Handler (FormResult x, Widget)
* getRouteToMaster is now called getRouteToParent. The rename comes because its semantics have changed slightly. (Perhaps we should still export a deprecated getRouteToMaster synonym?)
* Completely overhauled yesod-test, making it easier to use and providing cleaner integration with hspec.

__Persistent__

* Fields are now strict by default. To get lazy fields, add a ~ at the beginning of the field name.
* Remove the Join modules (you should use esqueleto instead)
* Refactored module hierarchy
* Renamed SqlPersist to SqlPersistT (former is kept as a deprecated synonym)
* Split up PersistField typeclass into PersistField and PersistFieldSql, the latter having the sqlType method.
* Much of the typeclass setup has been refactored to avoid the need passing undefined in the internals. No user-facing change should be involved here.
* If you define custom datatypes to be used by Persistent, they will usually need to be defined in a separate module due to the GHC stage restriction.

# 2013-02-18

* The Yesod ecosystem now supports conduit 1.0. This release of conduit is largely backwards compatible, and therefore most code will work with both conduit 0.5 and 1.0 (as is the case for Yesod itself). Little user modification should be necessary. For more information, see [the blog post](http://www.yesodweb.com/blog/2013/02/upcoming-conduit-1-0).
* wai 1.4 has been released, which adds a new field `requestBodyLength` to the `Request` datatype.
* monad-logger 0.3 is out, which no longer provides instances for `IO`. As a result, in a few places you may need to explicitly use a transformer to deal with logging. This will most commonly occur in the scaffolding with the runMigration call. The simplest thing to do is import `Control.Monad.Logger` and add `runNoLoggingT` to the beginning. For a more sophisticated approach, see [the changes to the scaffolding](https://github.com/yesodweb/yesod-scaffold/commit/578ae4068fd99c3a8ed219db75f477e44da484a4#L0R66).

# 2012-12-27

* yesod-core 1.1.7 has introduced a new means of creating session backends (clientSessionBackend2) and deprecated the old one. Going forward, the old method will be removed, which will allow us some significant performance optimizations. See: https://github.com/yesodweb/yesod/pull/418
* hamlet 1.1.3.1 now gives you an error message when a class name contains a hash, to avoid the problem of IDs accidentally being slurped up by class names. See: https://github.com/yesodweb/shakespeare/issues/75