# Init of the project

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
    Great, we'll be creating micropost today, and placing it in micropost.
    What's going to be the name of your foundation datatype? This name must
    start with a capital letter.

    Foundation: Micropost
    Yesod uses Persistent for its (you guessed it) persistence layer.
    This tool will build in either SQLite or PostgreSQL support for you.
    We recommend starting with SQLite: it has no dependencies.

    We have another option: a tiny project with minimal dependencies. In particular: no database and no authentication.

    So, what'll it be? s for sqlite, p for postgresql, t for tiny: s
    That's it! I'm creating your files now...
    Generating deploy/Procfile
    Generating config/sqlite.yml
    Generating config/settings.yml
    Generating main.hs
    Generating micropost.cabal
    Generating .ghci
    Generating LICENSE
    Generating Foundation.hs
    Generating Application.hs
    Generating Handler/Root.hs
    Generating Model.hs
    Generating Settings.hs
    Generating Settings/StaticFiles.hs
    Generating cassius/default-layout.cassius
    Generating hamlet/default-layout.hamlet
    Generating hamlet/boilerplate-layout.hamlet
    Generating static/css/normalize.css
    Generating hamlet/homepage.hamlet
    Generating config/routes
    Generating cassius/homepage.cassius
    Generating julius/homepage.julius
    Generating config/models

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

    The foundation for your site has been laid.


    There are a lot of resources to help you use Yesod.
    Start with the book: http://www.yesodweb.com/book
    Take part in the community: http://yesodweb.com/page/community


    Start your project:

       cd micropost && cabal install && yesod devel
    
A Yesod project is created with `yesod init` command. Yesod init asks 4 questions and here are the answers I gave:

1. Your name - davidbe
2. Project name - micropost
3. Foundation name - Micropost
4. Database-backend - s for sqlite

The Foundation is explained in the [Yesod book](http://www.yesodweb.com/book/basics).

After answering those questions, Yesod init will create several files. An explanation of all those files can be found in the [Yesod Web Framework Book](http://www.yesodweb.com/book) on [Scaffolding and the Site Template](http://www.yesodweb.com/book/scaffolding), but in this tutorial we will touch the most important ones.

Let's try the scaffolded code out:

    $ cd micropost
    $ cabal install
    $ yesod devel

[ Some guidance on how to handle cabal dependency errors might be in order at this point. For example, if you see "There is no available version of persistent-sqlite that satisfies ==0.6.*" then you should run "cabal install persistent-sqlite". Of course, I just realised that I'm only getting this because I normally run cabal site-wide, and the user under which I'm building my yesod projects doesn't even have a package list! Presumably if it did, cabal would install the missing dependencies. ]

When the console prints:

    Starting app

point the browser to `http://localhost:3000/`. A web-page with the following text will appear:

    Hello

    Added from JavaScript

    You are not logged in. Login now.

If that works, you know that everything is wired up correctly.
