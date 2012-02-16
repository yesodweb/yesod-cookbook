This is a public Yesod TODO list of tasks that are not yet underway unless noted, largely for those who are interested in contributing to Yesod. Please don't add anything onto here without discussing with Yesod contributors.

## Wiki

* better link assigning
* show diffs using the Diff or wordsetdiff package.


## testing infrastructure

* Try out doctest
* wai-test improvements
* develop a high-level haskell html/web application testing library or an interface to an existing one from a different language (Ruby's Capybara). Currently planning on using the shpider library.
* Yesod scaffolding generate testing files

## community infrastructure

* easy Yesod deployment. The ideal version would be something like Heroku for haskell. At a minimum we need a program to completely set up a vanilla Ubuntu install (either on EC2 or elsewhere) and sync & start a Yesod app. VMWare's cloud foundry is a very interesting deployment option. Someone is deploying a happstack app there.

## hamlet/templates

* a version of the hamlet template package that allows arbitrary haskell code in the templates. (might be easy)
* a simple pass-through html template (easy)

## Persistent

* All lower case (with underscores) model and column names so you don't have to quote everything when using the sql shell.
* sql statement logging, easy access to a logger in the persistent monad

Make it easier to put logic in the models

* model validations/callbacks
* tie that neatly into forms

## Development tools

* yesod console- load a ghci session with simple access to your Persistent models, printed out in a convenient (tabular?) format.
