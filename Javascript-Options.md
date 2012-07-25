Yesod by default gives you Shakespeare-Javascript, also known as Julius. It is just Javascript that you can insert haskell values into. Also Shakespeare-Coffee, which is Coffeescript you can insert haskell values into. Coffeescript is a thin veneer that papers over Javascript to make a convenient and consistent language, but there is still no type system. These can easily be used with any existing javascript code.

There is prior art for Julius: [jmacro](http://hackage.haskell.org/package/jmacro-0.5.1) has QQ value insertion, but also supports a haskell-ish version of javascript.

## Using Javascript Frameworks

* [Montage](http://hackological.com/blog/?p=5)


We are still hoping someone will figure out a satisfactory way to have strongly typed Javascript. Here are the options:

## Compile Haskell

* [Fay, Haskell code translated to JS](http://fay-lang.org/). Upside: produces a small amount of understandable javascript. Downside: only a subset of Haskell is supported.
* [UHC js compiler](http://www.haskell.org/haskellwiki/The_JavaScript_Problem#UHC). Downsides: alpha, not completely integrated with cabal, not GHC.
* [ghcjs](https://github.com/pedromartins/ghcjs) - [example Yesod app](https://github.com/hamishmack/yesod-slides) - directly compile ghc core to javascript. Downside: produces large amounts of javascript.

## Alternative experimental Haskell compilers

* [Emscripten](https://github.com/kripken/emscripten) — compiles LLVM/Clang output to JavaScript. If you use features of the GHC runtime you also have to compile it, which nobody has made an effort to figure out.
* [PNaCL](http://www.chromium.org/nativeclient/pnacl) - run native code on Google Chrome with NaCL. PNaCL is the next generation that will run LLVM byte code. Won't work on other browsers.
* [JS Haskell interpreter](https://github.com/johang88/haskellinjavascript)

## A Haskell-like language that only compiles to Javascript

* [Elm](http://elm-lang.org/) - Haskell inspired language that also offers an FRP GUI.
* [Roy](http://roy.brianmckenna.org/)
* [lambdascript](https://github.com/valderman/lambdascript)

## Haskell code that explicitly generates Javascript

* [Panther-Ajax](http://osdir.com/ml/general/2011-06/msg41431.html) Experimental library to write server side code that accesses the dom and uses continuations.
* [Ji](https://github.com/chrisdone/ji) - experimental library not currently working with Yesod. Same as Panther-Ajax, but under active development.
* [HJScript](http://hackage.haskell.org/package/HJScript-0.5.0) - javascript DSL.
* [yesod-js](https://github.com/snoyberg/yesod-js.git) - currently stalled while investigating more promising alternatives


## Add better typing to Coffeescript

[contracts.coffee](http://disnetdev.com/contracts.coffee/) not types, but contracts. Well maintained, easy to use. Firefox only (for development, in production contracts are turned off anyways).
[Uberscript](https://github.com/jstrachan/coffee-script/blob/master/TypeAnnotations.md) requires Google Closure compiler for its type-checking. A great idea, but this is a fork of coffeescript that has now fallen behind coffeescript by almost a year.


## Other strongly typed functional languages that compile to Javascript

* [Ur/Web](http://impredicative.com/ur/demo/) - A Haskell-like programming language tailored to web programming. Automatically generates FRP javascript code. Apropriate for taking the FRP plunge, but Heavyweight (large library and difficult to debug) if you just want some strongly-typed JS. [Opa](http://opalang.org) is a very similar concept.
* [JS of OCaml](http://ocsigen.org/js_of_ocaml/) - There is also an in-browser OCaml bytecode interpreter!