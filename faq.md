## Why does the Yesod install fail with "cannot find normal object file `dist/build/yesod/yesod-tmp/CodeGen.o'"?

The Yesod package includes an executable that uses some Template Haskell. Due to the way GHC works, there are a few hoops to jump through to get this to compile when profiling is enabled. To get around the issue, try installing Yesod with:

    cabal install yesod --disable-executable-profiling

## Why does Yesod have a special syntax for coding CSS and Javascript?

This practice is already common in the web development world, with Sass and Coffeescript. Here's a number of reasons why we do this in Yesod:


* Julius (Javascript) actually isn't a different syntax at all, it's
simply a pass-through parser.
* With Lucius and Cassius (for CSS), each one provides some syntactic
sugar over plain CSS that people like.
* All three languages allow for variable interpolation, including
type-safe URL interpolation.
* Lucius and Cassius automatically minify their output.
* We get many compile-time checks, but on variable existence and
correct typeness, and well-formedness of the file.
* These systems compose very nicely via widgets.
* And via these widgets, we're able to easily concatenate multiple
templates into a single file to be efficiently served via one HTTP
request.

## With "yesod devel" I get an error like "unknown symbol __stginit_foozm0zi0zi0_HandlerziFoo_"

Make sure that all your Handler files are listed in the cabal file, in the `other-modules` section.