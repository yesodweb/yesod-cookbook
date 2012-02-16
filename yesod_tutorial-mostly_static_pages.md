# Mostly static pages

## Goal

The goal of this chapter is the creation of simple, almost static pages. For our sample app, we want a Home, Contact, About and Help-page.

## Static pages with Yesod

The scaffold already created a homepage and configured a route for it.

### Homepage

The routing configuration can be found in `config/routes`. It looks like:

    /static StaticR Static getStatic
    /auth   AuthR   Auth   getAuth

    /favicon.ico FaviconR GET
    /robots.txt RobotsR GET

    / RootR GET

The last line creates a resource, named RootR, which accepts GET requests at the root (/) of our application. Yesod sees this resource declaration, and determines to call the handler function `getRootR`  whenever it receives a GET request for RootR. The function name follows the simple pattern of request method, in lowercase, followed by the resource name.

The handlers for the resources are defined in `Handler/`. The handler function `getRootR` is defined in `Handler/Root.hs`. See the code below. On line 13, you can find its definition. Line 15 is needed when the scaffolded authentication/authorization is wanted. There is [a chapter on authentication and authorization](http://www.yesodweb.com/book/authentication-and-authorization) in the Yesod book. We don't want it for now, so let's remove the line.

    {-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
    module Handler.Root where
    
    import Micropost
    
    -- This is a handler function for the GET request method on the RootR
    -- resource pattern. All of your resource patterns are defined in
    -- config/routes
    --
    -- The majority of the code you will write in Yesod lives in these handler
    -- functions. You can spread them across multiple files if you are so
    -- inclined, or create a single monolithic file.
    getRootR :: Handler RepHtml
    getRootR = do
        mu <- maybeAuth
        defaultLayout $ do
            h2id <- lift newIdent
            setTitle "micropost homepage"
            addWidget $(widgetFile "homepage")

The next lines combines different parts on how and what will be displayed on the root/homepage. First of all, there's `defaultLayout`. This will come back later, but this is where site-wide layout is defined.

Before I continue, it is necessary to know that Yesod uses four templating systems: Hamlet (HTML), Cassius (CSS), Lucius (CSS) and Julius (Javascript). Read the [Templates chapter](http://www.yesodweb.com/book/templates).

Next there's `h2id <- lift newIdent`. This does something with the javascript in the file `julius/homepage.julius`. `newIdent` generates unique identifiers.

TODO: A better explanation on this and on newIdent.
see: [Book: Generate IDs](http://www.yesodweb.com/book/widgets#file277-generate-ids)

`setTitle "micropost homepage"` is easy: it sets the title of the web page.

Last but not least `addWidget $(widgetFile "homepage")`. The chapter about [widgets](http://www.yesodweb.com/book/widgets) will make many things clear. `widgetFile` is defined in `config/Settings.hs` and combines - if they exist - `hamlet/homepage.hamlet, cassius/homepage.cassius, julius/homepage.julius` and `lucius/homepage.lucius`. It is in these files that we write our html, css and javascript.

Okay, time to adjust the default homepage! We want to change our title to "Yesod Tutorial Micropost | Home". Change the title in `Handler/Root.hs`. The function `getRootR` will look like this:

    getRootR :: Handler RepHtml
    getRootR = do
        mu <- maybeAuth
        defaultLayout $ do
            h2id <- lift newIdent
            setTitle "Yesod Tutorial Micropost | Home"
            addWidget $(widgetFile "homepage")

Next, edit `hamlet/homepage.hamlet` and change it into:

    <h1>Micropost
    <p>This is the homepage for the #
        <a href="http://www.yesodweb.com">Yesod tutorial
        \ sample application.

This is Hamlet template code. It is very close to html, so it is all understandable. Some highlights:

* no closing tags, nesting is determined by indention lever
* everything written after `#` is omitted, but this makes you sure that there's a space after `the`.
* the same goes for white space at the beginning of a line, therefore a backslash is used `\`

Refresh the browser on url `localhost:3000` and you'll get:

    Micropost

    This is the homepage for the Yesod tutorial sample application.

### Contact

Now our contact-page. We have to define our resource in `config/routes`, create a handler file for the resource `Handler/Contact.hs`, get the handler-file in scope, write a .hamlet-file in `hamlet/` and add the handler to the cabal-file.

Edit `config/routes` and add the line:

    /contact ContactR GET

Next, create a file `Handler/Contact.hs` and paste this code into it:

    {-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
    module Handler.Contact where

    import Foundation

    getContactR :: Handler RepHtml
    getContactR = do
        defaultLayout $ do
            setTitle "Yesod Tutorial Micropost | Contact"
            addWidget $(widgetFile "contact")

This handler-file should be imported in `Application.hs` so it gets in scope. Add `import Handler.Contact` under these lines:

    -- Import all relevant handler modules here.
    import Handler.Root

Almost there... Create the file `hamlet/contact.hamlet` with this content:

    <h1>Contact
    <p>Contact Yesod Tutorial at the #
        <a href="http://www.haskell.org/mailman/listinfo/web-devel"> Haskell web-devel mailing-list
        \.

And finally, you need to add this line to your `micropost.cabal`-file, at section `other-modules` and under `Handler.Root`, so that cabal includes this file in the project:

    Handler.Contact

### About and Help

The about and help page are created almost identical to the contact page and should be a good exercise. The about page should display:

    About

    The Yesod Tutorial is a project to make a small, beginner level, tutorial, to teach how to use Yesod Web Framework for web development.

with `Yesod Web Framework` linking to the homepage of Yesod. The help page should be like:

    Help

    Get help on Yesod Tutorial at the Yesod community.

with `Yesod community` as a link to (http://www.yesodweb.com/page/community).

## DRY

Now that we've created some static pages, you might have noticed that we've been repeating some text. In every handler, we have set the title with following structure:

    "Yesod Tutorial Micropost | " & "Home"
    "Yesod Tutorial Micropost | " & "Contact"
    "Yesod Tutorial Micropost | " & "About"
    "Yesod Tutorial Micropost | " & "Help"

If this is the default structure of the title on any page, it should be made default. In every handler, we saw `defaultLayout`. This function does different things, it also applies the default .hamlet, .cassius, .lucius and .julius. These files are named `default-layout.*`.

The content of `hamlet/default-layout.hamlet` is shown below. You'll notice the structure out-line of every .html-document (in .hamlet syntax). There are some special variables (#, ^ and $). #, ^ and also @ are interpolation characters and are always followed by the variable inside braces. The hash is used for variable interpolation, at-sign (@) for URL interpolation, and caret (^) for embedding. $ allows control structures inside hamlet. Read [the Template chapter](http://www.yesodweb.com/book/templates).

    !!!
    <html
        <head
            <title>#{pageTitle pc}
            <link rel="stylesheet" href=@{StaticR css_normalize_css}>
            ^{pageHead pc}
        <body
            $maybe msg <- mmsg
                <div #message>#{msg}
            ^{pageBody pc}

To achieve our goal, we only need to change title line into

            <title>Yesod Tutorial Micropost | #{pageTitle pc}

and remove, in every handler file we've written, `Yesod Tutorial Micropost | ` from the parameter of `setTitle` function, so it is as simple is `setTitle "Home"`.

Having done all that, how can you see the results of your hard work? Run `yesod build` to recompile the project, and `cabal install` to install it.