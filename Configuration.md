## Overriding configuration values with environment variables

Since 1.4.1, Yesod has supported overriding configuration file values with an environment variable by using the `_env:ENV_VAR_NAME:default_value` syntax, like so:

```yaml
port: "_env:PORT:3000"
```

In this example, `port` will default to 3000 but can be overridden by the `PORT` environment variable.

Using environment variables is optional, but gives you the flexibility to override settings across your production, staging, development, and other environments. Environment variables scale better than per-environment configuration files, which each require a unique name and a new config file. Additionally, environment variable names can be standardized; for example, Keter, FP Haskell Center, `yesod devel` and Heroku all set the `PORT` environment variable for use by your app.

See http://12factor.net/config for more on this topic.

### Parsing numeric values as Strings

If your default value is numeric (e.g. `"_env:PORT:3000"`), it will be parsed as a Number. If you need it to be parsed as a String, wrap it in single quotes:

```yaml
database:
  user:     "_env:PGUSER:postgres"
  password: "_env:PGPASS:'123'"
```

> You may encounter an error like "Could not convert to AppSettings: Persistent: error loading PostgreSQL conf: when expecting a String, encountered Number instead" if the numeric value isn't quoted.