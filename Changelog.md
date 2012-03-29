# Changelog

## Yesod 1.0

* Generalized yepnopeJs to jsLoader
* Generalized encryptKey to makeSessionBackend
* Hamlet tags now require closing angle bracket, allow multiline tags.
* Moved to conduit 0.4
* yesod-test
* yesod-generate (external)
* `fsClass` in `FieldSettings` has been replaced by the more general purpose `fsAttrs`.

### Shakespeare

* Support Roy.js
* Improved Coffeescript support (better job dealing with comments)
* Hamlet now supports `*{attrs}` syntax within a tag.
* Removed support for infix operators.

### Persistent

* Moved to conduit 0.4

### WAI

* Moved to conduit 0.4