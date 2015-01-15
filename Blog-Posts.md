* http://yannesposito.com/Scratch/en/blog/Yesod-tutorial-for-newbies
[acid-state to create a url shortener](http://flygdynamikern.blogspot.com/2011/06/toy-url-shortener-with-yesod-and-acid.html)
* [testing with selenium and feature tests](http://gabrielprioli.wordpress.com/2012/09/11/using-a-business-readable-language-for-browser-automation/)
* [Using acid-state for persistence](http://meadowstalk.com/post/migration-to-acid-state)
* [Pieces of Yesod: Inverting a Haskell Function, a DRY way to convert some haskell types to a url piece](http://chplib.wordpress.com/2011/06/17/pieces-of-yesod-inverting-a-haskell-function/)
* [Deploying to Amazon with Fedora](http://jpmoresmau.blogspot.com/2011/04/install-ghc7-and-yesod-on-amazon-linux.html)
* [Dropbox API with Haskell](http://tech.dropbox.com/?p=129)
* [Internationalization](http://jabberwocky.eu/2014/02/10/internationalization-with-yesod/)


# Blog Post Ideas

A list of blog post ideas to increase content for non-haskellers.

- Building Wai/Yesod Applications! 

    We've heard a lot about libraries :). We could talk more about what 
    we are building and how. We have the wiki page 
    https://github.com/yesodweb/yesod/wiki/Powered-by-Yesod but I only 
    recently found this myself. 


- Refactoring a route (or model) in an application. 

    Common pain-point in other frameworks is refactoring. I want to make 
    core modifications to a web app; large or small, old or new. this 
    can be quite fun in haskell and we should present it as so. 


- Deploying apps to Aws,Heroku,DigitalOcean,wherever; possibly with Docker (or some other container solution). 

   This has been discussed many times and will probably always be on 
   peoples minds. Given the rapid pace of these technologies it would be 
   nice to talk about deployment in light of what _we_ use as opposed to 
   simply POC. What are Haskell developers using in production today? 


- More Javascript interoperability 

  Not necessarily everyone's cup of tea but there is a breadth of 
  Javascript libraries out there people like and are familiar with and 
  would like to use. 


- Wai/Yesod microservices interacting with other services written with 
  different tools. 


- Optimizing Persistent (when to use Esqueleto or raw sql). 

   I have some apps with quite a few large "normalized" tables which 
   require several get, getBy, select calls to pull an object. What are 
   best practice for compensating the areas where Persistent might be 
   lacking (e.g. joins). 