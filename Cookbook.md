# The Yesod Cookbook

The Yesod Cookbook contains small snippets of code for solving practical problems with Yesod. If you have some code to share, please create a wiki page and link to it in the appropriate section.

If you're looking for a comprehensive guide to learning Yesod, please consider [reading the book](http://www.yesodweb.com/book).

For non-Yesod code, see also [Snippets](https://github.com/yesodweb/yesod-cookbook/blob/master/Snippets.md).

## Basics
* [Adding CSS and JS](http://www.yesodweb.com/blog/2013/01/adding-css-js)
* [JSON Object Create and Append](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/JSON-data-helpful-manipulations.md)
* [Doing AJAX calls with CSRF Protection](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/ajax-csrf.md)
* [Develop JS without recompiling the yesod project](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Develop-JS-without-recompiling-the-yesod-project.md)
* [RAW JSON Parsing example](./cookbook/raw-json-parsing.md)

## Forms

* [Change the order of form fields](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/OrderFormFields.md)
* [Require input for Maybe field](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/RequireInputMaybeField.md)
* [Custom form layout](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/CustomFormLayout.md)
* [Password confirm field](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Password-Confirm-Field.md)
* [Multi-field validation](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Cookbook-Multi-field-validation.md)
* [Select field populated from database](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Select-field-populated-from-database.md)
* [Multi-select field populated from database](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Multi-select-field-populated-from-database.md)
* [Multi-select field populated from database (esqueleto)](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Multi-select-field-populated-from-database-(using-esqueleto).md) - this is the optimized version
* [File upload](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Cookbook-File-upload.md)
* [File upload saving files to server](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Cookbook-file-upload-saving-files-to-server.md)

## Persistence

* [Run raw MongoDB](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Rawmongo.md)
* [Run raw SQL](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/RawSQL.md)
* [Sample MongoDB connection with persistent](./cookbook/mongodb-example.md)
* [Sphinx Search](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Sphinx-Search.md)
* [Non scaffolded MongoDB App](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Non-scaffolded-MongoDB-App.md)
* [Using Database.Persist.runPool without Foundation](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Using-Database.Persist.runPool-without-Foundation.md)
* [Using runDB in IO with SQL ](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/runDBConf-in-IO-for-MySQL.md)
* [Database Views and Non-Migrated Entities](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Database-Views-and-Non-Migrated-Entities.md)
* [Lens for existing site](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Lens-for-existing-site.md)
* [Connecting to an additional existing database](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Connecting-to-an-additional-existing-database.md)
* [Example MySQL Connection code](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Example-MySQL-Connection-code.md)
* [Example Postgres Connection code](./cookbook/postgres-example-code.md)
* [Activate foreign key checking in Sqlite](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Activate-foreign-key-checking-in-Sqlite.md)
* [Adding Seed data to Scaffolded Site](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Adding-Seed-Data-to-Scaffolded-Site.md)
* [Handling exception in persistence](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Handling-Persistence-Exception.md)
* [Builtin Postgresql types with Persistent](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/CustomPostgresqlTypes.md)
* [Modelling Schema with Persistent](./cookbook/modelling-schema.md)
* [Custom data types column in Persistent](./cookbook/custom-datatype-columns.md)

## State

* [Keeping (in-memory) state with warp](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Keeping-(in-memory)-state-with-warp.md)
* [Keeping (in-memory) state with yesod](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Keeping-(in-memory)-state-with-yesod.md)

## HTML5 and Yesod
* [Basic ServerEvent example](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Basic-ServerEvent-example.md)
* [Allowing WOFF fonts to be accessed from other domains (CORS)](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Allowing-WOFF-fonts-to-be-accessed-from-other-domains-(CORS).md)

## Auth

* [Use HashDB in a scaffolded site](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Using-HashDB-In-a-Scaffolded-Site.md)
* [Abstracting user roles & permissions](http://blog.felipe.lessa.nom.br/?p=7)
* [Demo of Yesod.Auth.Email module using JSON endpoints](./cookbook/yesod-auth-json.md)

## Internationalization

* [Internationalization of forms](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Cookbook-InternationalizedForm.md)

## Routing

* [Hierarchical routes and breadcrumbs](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Hierarchical-routes-and-breadcrumbs.md)
* [Domain-based routing](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Domain-based-routing.md)
* [Static file subsite Hello World](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Static-file-subsite-Hello-World.md)
* [Pure-Haskell static subdomains](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Pure-Haskell-static-subdomains.md)
* [Slugs](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Slugs.md)
* [Using type-safe URLs from inside Javascript](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Using-type-safe-urls-from-inside-javascript.md)
* [Subsite with typeclass contexts and type variables](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Subsite-with-typeclass-contexts-and-type-variables.md)

## Ajax

* [Using Ajax with a Scaffolded Site](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Using-Ajax-with-a-Scaffolded-Site.md)
* [Convert Get Params into a haskell record ](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Convert-get-params-into-a-haskell-record.md)

## Testing

* TODO: Testing Yesod App
* [Performing Authentication during Testing](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Performing-Authentication-during-Testing.md)
* [Flushing Database Between Tests](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Flushing-database-between-tests.md)

## Template Haskell

* [Creating string based enums using Template Haskell](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Create-String-Based-Enums-With-Template-Haskell.md)

## Conduit

* [Consume a webpage with http-conduit and print progress along the way](https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/Consume-a-webpage-with-http-conduit-and-print-progress-along-the-way.md)

## Heroku

* [Deploying to Heroku](./cookbook/yesod-heroku-deploy.md)
