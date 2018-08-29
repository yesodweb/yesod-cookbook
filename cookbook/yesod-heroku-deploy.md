# Heroku Deployment

This tutorial will go over the steps needed in order to get a Yesod application
deployed to Heroku, connecting to a database, and ensuring all requests are
forced to use the SSL protocol.

We are going to assume two things in this tutorial:

1. You have used Heroku before to deploy web applications.
1. You have a Yesod application created with the `yesod-postgres` template.

## Adding a Buildpack

The first thing we are going to want to do is to add a buildpack for Haskell. I
have had a good experience with the
[heroku-buildpback-stack](https://github.com/mfine/heroku-buildpack-stack).

You can add a buildpack via Heroku's web UI, or on the command line:

```bash
heroku buildpacks:set https://github.com/mfine/heroku-buildpack-stack
```

## Binding to the Correct Port

The port that a Heroku application must bind to [is set
dynamically](https://devcenter.heroku.com/articles/dynos#web-dynos) by Heroku,
but can be accessed via the `PORT` environmental variable.

Yesod already has a mechanism for retrieving the port to bind to from the
environment, but it looks for the `YESOD_PORT` environmental variable.

What we need to do, then is to set `YESOD_PORT` equal to the value of `PORT`
before the application is started. All we have to do is add a `.profile` file to
the root of our Yesod project:

```
# .profile
export YESOD_PORT=$PORT
```

## Adding a Database

Adding a database happens in two steps.

First, we need to add the "addon" to our application:

```
heroku addons:create heroku-postgresql:hobby-dev
```

Next, we need to instruct Yesod to connect to the database server that Heroku
has provisioned for us.

If you run `heroku config`, you will see all the environmental variables in our
environment. We are only interested in `DATABASE_URL`, which will be a Postgres
connection string in the following format:
`postgres://<user>:<pass>@<host>:<port>/<db>`.

If you take a look at `./config/settings.yml`, you can see that Yesod looks for
specific environmental variables. Let's set these on Heroku so that Yesod will
pick them up:

```
heroku config:set YESOD_PGUSER=<user> YESOD_PGPASS=<pass> YESOD_PGHOST=<host> YESOD_PGDATABASE=<database>
```

## Forcing SSL

A common requirement in production applications is to force all connections to
an application to be made via SSL / the HTTPS protocol.

While some of that can be done at the DNS level, it is [the responsibility of
the application
layer](https://help.heroku.com/J2R1S4T8/can-heroku-force-an-application-to-use-ssl-tls)
to do that.

Luckily, Yesod is built on WAI, and WAI provides the
[forceSSL](https://www.stackage.org/haddock/lts-12.8/wai-extra-3.0.24.2/Network-Wai-Middleware-ForceSSL.html#v:forceSSL)
that will do exactly that.

We can modify `makeApplication` function, and simply include the middleware.
(Make sure to import `forceSSL`).

```haskell
-- src/Application.hs

makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ forceSSL $ logWare $ defaultMiddlewaresNoLogging appPlain
```

However, what if we only wanted to force SSL in production? Well, notice how
`makeApplication` is called with a value of type `App`, and `App` is created in
the `makeFoundation` functions. We can add a new field to `App`, initialize that
field in `makeFoundation`, and then only use `forceSSL` if that field's value is
`Production`.


```haskell
-- src/Foundation.hs

data Environment = Production | Development deriving (Read)

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , environment    :: Environment -- ^ This is our new field
    }

-- src/Application.hs

-- In the makeFoundation, we want to make sure that our environment field is
-- initialized
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    --
    menv <- readMay <$> getEnv "YESOD_ENVIRONMENT"
    let environment = maybe Development id menv -- default to Development
    --

-- Finally, we switch forceSSL on or off based on the value of the environmen
-- field of our App value.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    let sslWare = makeSSLWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ sslWare $ logWare $ defaultMiddlewaresNoLogging appPlain

makeSSLWare :: App -> Middleware
makeSSLWare App{..} = case environment of
                          Production -> forceSSL
                          Development -> id
```

Now, we need to make sure that whenever we start our application we have the
`YESOD_ENVIRONMENT` set to either `Production` or `Development`.

### Alternatives

Note: If the above works for your sensibilities, don even bother reading the
below.

There are a few different ways that we could have gone about conditionally
turning on `forceSSL`.

Since `makeApplication` is already an IO action, we could have read the
environment within `makeApplication`. I like to keep the IO closer to the top
layers of the system, so I didn't choose this route.

Another option is to add a field to the `AppSettings` data type, and use the
`dev` variable that is set to `True` or `False` depending on how the system was
started. If we went this route, we'd have to change another part of the system
so that the application would be started in such a way that dev would be defined
as true when I deployed to my staging server in Heroku.

Finally, we could have used the `settings.yml` instead, since we are already
using an environmental variable named `YESOD_`. I think this is a legitimate
route, but I didn't think about it until later. I'll update this guide if I ever
get around to it.

Point is, the suggested route is "good enough".
