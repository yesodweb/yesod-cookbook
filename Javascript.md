Yesod by default gives you Shakespeare-JS, formerly known as Julius. It is just Javascript that you can insert haskell values into. Also Shakespeare-Coffee, which is Coffeescript you can insert haskell values into.

Other options:


* [jmacro](http://hackage.haskell.org/package/jmacro-0.5.1) a quasi-quoter that allows insertion of haskell values into raw javascript like Julius. Also supports a haskell-like version of javascript.
* [ghcjs](https://github.com/pedromartins/ghcjs) - [example Yesod app](https://github.com/hamishmack/yesod-slides) - directly compile ghc code to javascript!
* [lambdascript]( https://github.com/aculich/lambdascript) - directly compile a subset of haskell to javascript
* [HJScript](http://hackage.haskell.org/package/HJScript-0.5.0) - javascript DSL.
* [Panther-Ajax](http://osdir.com/ml/general/2011-06/msg41431.html) Experimental library to write server side code that accesses the dom and uses continuations.
* [Emscripten](https://github.com/kripken/emscripten) — compiles LLVM/Clang output to JavaScript. If you use features of the GHC runtime you also have to compile it, which nobody has made an effort to figure out.
