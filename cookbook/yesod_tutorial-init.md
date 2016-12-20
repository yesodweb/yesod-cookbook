# Init of the project

**Warning**: Stack templates are now the preferred way of creating scaffolded
Yesod apps, use `stack templates` to lists the available ones. Additionally use
`stack exec -- yesod` to invoke *yesod-bin* in your project.

## Goal

The goal of this chapter is to create the scaffolded site and make it work.

## Init

First we need to create a new Yesod project, let's call it `micropost`:

    $ yesod init
    Welcome to the Yesod scaffolder.
    I'm going to be creating a skeleton Yesod project for you.

    What is your name? We're going to put this in the cabal and LICENSE files.

    Your name: davidbe
    Welcome davidbe.
    What do you want to call your project? We'll use this for the cabal name.

    Project name: micropost
    Yesod uses Persistent for its (you guessed it) persistence layer.
    This tool will build in either SQLite or PostgreSQL or MongoDB support for you.
    We recommend starting with SQLite: it has no dependencies.

        s     = sqlite
        p     = postgresql
        mongo = mongodb
        mysql = MySQL (experimental)

    So, what'll it be? s
    That's it! I'm creating your files now...
    Generating deploy/Procfile
    Generating config/sqlite.yml
    Generating config/settings.yml
    Generating main.hs
    Generating devel.hs
    Generating micropost.cabal
    Generating micropost.cabal
    Generating .ghci
    Generating LICENSE
    Generating Foundation.hs
    Generating Import.hs
    Generating Application.hs
    Generating Handler/Home.hs
    Generating Model.hs
    Generating Settings.hs
    Generating Settings/StaticFiles.hs
    Generating static/css/bootstrap.css
    Generating templates/default-layout.hamlet
    Generating templates/default-layout-wrapper.hamlet
    Generating templates/normalize.lucius
    Generating templates/homepage.hamlet
    Generating config/routes
    Generating templates/homepage.lucius
    Generating templates/homepage.julius
    Generating config/models
    Generating messages/en.msg
    Generating tests/main.hs

    ---------------------------------------
    
                             ___
                                {-)   |\
                           [m,].-"-.   /
          [][__][__]         \(/\__/\)/
          [__][__][__][__]  |  |
          [][__][__][__][__][] /   |
          [__][__][__][__][__]| /| |
          [][__][__][__][__][]| || |  
      ejm [__][__][__][__][__]__,__,  \__/

    
    ---------------------------------------
    
    The foundation for your web application has been built.
    
    
    There are a lot of resources to help you use Yesod.
    Start with the book: http://www.yesodweb.com/book
    Take part in the community: http://yesodweb.com/page/community
    
    
    Start your project:
    
       cd micropost && cabal install && yesod devel
     
    or if you use cabal-dev:
    
       cd micropost && cabal-dev install && yesod --dev devel

    
A Yesod project is created with `yesod init` command. Yesod init asks 3 questions and here are the answers I gave:

1. Your name - davidbe
2. Project name - micropost
3. Database-backend - s for sqlite

After answering those questions, Yesod init will create several files. An explanation of all those files can be found in the [Yesod Web Framework Book](http://www.yesodweb.com/book) on [Scaffolding and the Site Template](http://www.yesodweb.com/book/scaffolding), but in this tutorial we will touch the most important ones.

Let's try the scaffolded code out:

    $ cd micropost
    $ cabal install
    $ yesod devel

[ Some guidance on how to handle cabal dependency errors might be in order at this point. For example, if you see "There is no available version of persistent-sqlite that satisfies ==0.6.*" then you should run "cabal install persistent-sqlite". Of course, I just realised that I'm only getting this because I normally run cabal site-wide, and the user under which I'm building my yesod projects doesn't even have a package list! Presumably if it did, cabal would install the missing dependencies. ]

When the console prints:

    Devel application launched: http://localhost:3000

point the browser to `http://localhost:3000/`. A web-page with explaining the first steps with Yesod will appear.

If that works, you know that everything is wired up correctly.
