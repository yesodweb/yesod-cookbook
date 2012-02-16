# Filling in the layout

## Goal

The goal of this chapter is to improve the look and feel of our pages and to make them interlinked.

## Cascading Style Sheets - framework

Our sample application will get some minimal styling. Styling is done with CSS. As written before, Yesod has two different formatting available for CSS: cassius and lucius. The difference is that cassius syntax is different from CSS in the same way hamlet is different from html: use of indention, less syntactic symbols, ... Lucius on the other hand, should be able to interpret standard CSS. Though, directly using CSS is still possible.

In this sample application, we will use cassius, but as a foundation for our styling, we will use the [Blueprint CSS framework](http://www.blueprintcss.org/). This might sound like a strange twist in the tutorial: in stead of starting with the template-syntax, we follow a more difficult road. In my opinion using a CSS framework is not a bad idea and installing it should be the a thing to do before customizing it.

Download [the latest Blueprint CSS](http://github.com/joshuaclayton/blueprint-css/zipball/master). Unzip the file and copy the complete directory `blueprint` from the zip-file into `micropost/static/`.

The `static/` directory is a special directory created with the scaffold. As you have seen, there's also a resource created in `config/routes`. As I understand, `static/` is the place to put all non- Yesod & Haskell stuff needed for your website.

The stylesheet is now available and should be added to our default layout. Open up `hamlet/default-layout.hamlet`. Add these two lines in the `<head`-section before `^{pageHead pc}`:

            <link rel=stylesheet media=screen href=@{StaticR blueprint_screen_css}>
            <link rel=stylesheet media=print href=@{StaticR blueprint_print_css}>

Some explanation. The `@{…}` is an interpolation and generates an URL. An URL based on `StaticR`'s parameter. The parameter is the path to the file, including extension, where `/` and `.` are replaced by underscores.

These lines need to be before `^{pageHead pc}`. We first want that all the rules defined in the Blueprint CSS are processed and after that, our cassius and lucius definitions.

There is one downside to this approach that needs to be mentioned: if you change files in your static folder without modifying the module that calls staticFiles, you will still have the old identifiers in your object files. I recommend having a StaticFiles module in each project that just runs the staticFiles function. Whenever you modify your static folder, touch StaticFiles.hs and you should be good to go.

So, run this command in the console:

    touch config/StaticFiles.hs

When you refresh one of the four pages, you'll notice a change in font!

## Adding some structure

We want to give some more structure to our pages with a site logo, a navigation header and a site footer. The mockup looks like [http://railstutorial.org/images/figures/home_page_mockup-full.png](http://railstutorial.org/images/figures/home_page_mockup-full.png).

The site navigation should be omnipresent and therefore added to `hamlet/default-layout.hamlet`.

Change the content into:

    !!!
    <html
       <head
          <title>Yesod Tutorial Micropost | #{pageTitle pc}
          <link rel=stylesheet media=screen href=@{StaticR blueprint_screen_css}>
          <link rel=stylesheet media=print href=@{StaticR blueprint_print_css}>
          ^{pageHead pc}
       <body
          <div .container
               <header
                  <img border="0" src=@{StaticR img_yesod_png} alt="Sample App in Yesod" .round
                  <nav .round
                       <ul
                          <li
                             <a href=@{RootR}>Home
                          <li
                             <a href=@{HelpR}>Help
                          <li
                             <a href="#">Sign in
               <section .round
                        ^{pageBody pc}
               <footer
                  <nav .round>
                    <ul
                       <li
                          <a href=@{AboutR}>About
                       <li
                          <a href=@{ContactR}>Contact

This is pretty straightforward HTML5 in hamlet syntax, with some URL-interpolations. The link to "Sign in" isn't available yet.

I made a sub-directory `img/` in `static/` and made a simple logo called `yesod.png`. After adding that, don't forget to `touch config/StatifFiles.hs`!

You'll notice that there's some difference between the placement of the main title on the Home-page and on other pages. This is because the scaffold already generates a .cassius-file for `homepage.hamlet`. It is safe to remove the content of that `cassius/homepage.cassius`.

## Default custom CSS

We first implemented Blueprint CSS as our CSS foundation, but we want to alter some things site wide. Therefore we need to edit `cassius/default-layout.cassius`. Change the content with this:

~~~ {.css}
    .container
       width: 710px

    body
        background: #cff

    header
        padding-top: 20px

    header img
        padding: 1em
        background: #fff

    section
        margin-top: 1em
        font-size: 120%
        padding: 20px
        background: #fff

    section h1
        font-size: 200%

    /*links*/

    a
        color: #09c
        text-decoration: none

    a:hover
        color: #069
        text-decoration: underline

    a:visited
        color: #069

    /* Navigation */

    nav
        float: right

    nav
        background-color: white
        padding: 0 0.7em
        white-space: nowrap

    nav ul
        margin: 0
        padding: 0

    nav ul li
        list-style-type: none
        display: inline-block
        padding: 0.2em 0

    nav ul li a
        padding: 0 5px
        font-weight: bold

    nav ul li a:visited
        color: #09c

    nav ul li a:hover
        text-decoration: underline

    /* Round corners */

    .round
        -moz-border-radius: 10px
        -webkit-border-radius: 10px
        border-radius: 10px

    /* Footer */

    footer
        text-align: center
        margin-top: 10px
        width: 710px
        margin-left: auto
        margin-right: auto

    footer nav
        float: none
~~~

This is cassius syntax, like hamlet, it is based on indentation and so doesn’t require characters: `{};` . I also could have placed normal CSS in `lucius/default-layout.lucius` which has the same result.

## A big green sign up button

We have a sign in link in the top navigation bar, but no link for signing up! This should be placed in the homepage. Add this to the end of `hamlet/homepage.hamlet`.

    <a href="#" .signup_button .round>Sign up now!

Because the sign up button is specific to the homepage, we can put the CSS into `cassius/homepage.cassius`:

    /* Sign up button */

    a.signup_button
        margin-left: auto
        margin-right: auto
        display: block
        text-align: center
        width: 190px
        color: #fff
        background: #006400
        font-size: 150%
        font-weight: bold
        padding: 20px

Notice that putting this code in `cassius/default-homepage.cassius` will also work and will be available on every page.

The homepage will look like [http://railstutorial.org/images/figures/site_with_footer-full.png](http://railstutorial.org/images/figures/site_with_footer-full.png).
