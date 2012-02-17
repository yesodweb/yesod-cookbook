This is a public Yesod TODO list of tasks that are not yet underway unless noted, largely for those who are interested in contributing to Yesod. Please don't add anything onto here without discussing with Yesod contributors.
see [[Wish List]]

## testing infrastructure

* Try out doctest

## community infrastructure

* easy Yesod deployment. The ideal version would be something like Heroku for haskell. At a minimum we need a program to completely set up a vanilla Ubuntu install (either on EC2 or elsewhere) and sync & start a Yesod app. VMWare's cloud foundry is a very interesting deployment option. Someone is deploying a happstack app there.


## Persistent

* sql statement logging, easy access to a logger in the persistent monad

Make it easier to put logic in the models

* model validations/callbacks
* tie that neatly into forms
