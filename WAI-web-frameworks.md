Yesod works on top of WAI, the Web Application Interface. This allows the user to deploy to different targets with minimal (if any) changes, but more importantly lets different web frameworks share code, even when they have very different aims.

Yesod does not attempt to revolutionize the web framework concept. Instead it applies Haskell's strengths to normal REST and MVC style web development. In contrast, there are new web frameworks with very different approaches. Both use WAI and Hamlet.

* [dingo](http://hackage.haskell.org/package/dingo-core) - GUI-style code
* [webwire](http://hackage.haskell.org/package/webwire) -  FRP code

Here is another WAI based framework.

* [Scotty](https://github.com/xich/scotty) -  Sinatra (simple Ruby web framework) inspired. Consider using it if you have a simple site and no interest in Yesod's Widgets and various other features.

Adapters

* [happstack-wai](https://github.com/aslatter/happstack-wai.git)