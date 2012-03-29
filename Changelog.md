# Changelog

## Yesod 1.0

* Generalized yepnopeJs to jsLoader
* Generalized encryptKey to makeSessionBackend
* Upgrade to conduit 0.4
* yesod-test
* yesod-generate (external)
* `fsClass` in `FieldSettings` has been replaced by the more general purpose `fsAttrs`.

### Shakespeare

* Hamlet tags now require closing angle bracket, allow multiline tags.
* Hamlet now supports `*{attrs}` syntax within a tag.
* Removed support for infix operators.

* Support Roy.js
* Improved Coffeescript support (better job dealing with comments)


### Persistent

* Upgrade to conduit 0.4

### WAI

* Upgrade to conduit 0.4

### Scaffolding

* Removed tiny option
* Including bootstrap.css
* yesod-test tests included
* Homepage is a very small tutorial