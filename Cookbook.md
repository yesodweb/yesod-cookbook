# The Yesod Cookbook

The Yesod Cookbook contains small snippets of code for solving practical problems with Yesod. If you have some code to share, please create a wiki page and link to it in the appropriate section.

If you're looking for a comprehensive guide to learning Yesod, please consider [reading the book](http://www.yesodweb.com/book).

For non-Yesod code, see also [[Snippets|Snippets]].

## Basics
* [Adding CSS and JS](http://www.yesodweb.com/blog/2013/01/adding-css-js)
* [[JSON Object Create and Append | JSON-data-helpful-manipulations]]
* [[Develop JS without recompiling the yesod project]]

## Forms

* [[Change the order of form fields|OrderFormFields]]
* [[Require input for Maybe field|RequireInputMaybeField]]
* [[Custom form layout|CustomFormLayout]]
* [[Password confirm field|Password Confirm Field]]
* [[Multi-field validation|Cookbook/Multi-field validation]]
* [[Select field populated from database|Select-field-populated-from-database]]
* [[Multi-select field populated from database|Multi-select-field-populated-from-database]]
* [[Multi-select field populated from database (esqueleto)|Multi-select-field-populated-from-database-(using-esqueleto)]] - this is the optimized version
* [[File upload|Cookbook/File upload]]
* [[File upload saving files to server|Cookbook/File upload saving files to server]]

## Persistence

* [[Run raw MongoDB|RawMongo]]
* [[Run raw SQL|RawSQL]]
* [[Sphinx Search|Sphinx Search]]
* [[Non scaffolded MongoDB App|Non scaffolded MongoDB App]]
* [[Using Database.Persist.runPool without Foundation|Using Database.Persist.runPool without Foundation]]
* [[Using runDB in IO with SQL | runDBConf in IO for MySQL]]
* [[Database Views and Non-Migrated Entities]]
* [[Lens for existing site]]
* [Connecting to an additional existing database](https://github.com/yesodweb/yesod/wiki/Connecting-to-an-additional-existing-database)
* [Example MySQL Connection code](https://github.com/yesodweb/yesod/wiki/Example-MySQL-Connection-code)
* [[Activate foreign key checking in Sqlite]]
* [[Adding Seed data to Scaffolded Site]]

## State

* [[Keeping (in-memory) state with warp]]
* [[Keeping (in-memory) state with yesod]]

## HTML5 and Yesod
* [[Basic ServerEvent example]]
* [[Allowing WOFF fonts to be accessed from other domains (CORS)]]

## Auth

* [[Use HashDB in a scaffolded site|Using HashDB In a Scaffolded Site]]
* [Abstracting user roles & permissions](http://blog.felipe.lessa.nom.br/?p=7)

## Internationalization

* [[Internationalization of forms|Cookbook/InternationalizedForm]]

## Routing

* [[Hierarchical routes and breadcrumbs|Hierarchical-routes-and-breadcrumbs]]
* [[Domain-based routing|Domain-based routing]]
* [subdomain (www) redirect](/show/topic/536)
* [[Static file subsite Hello World|Static-file-subsite-Hello-World]]
* [[Pure-Haskell static subdomains|Pure-Haskell static subdomains]]
* [[Slugs]]
* [[Using type-safe URLs from inside Javascript]]

## Ajax

* [[Using Ajax with a Scaffolded Site|Using Ajax with a Scaffolded Site]]
* [[Convert Get Params into a haskell record ]]

## Testing

* [[Testing Yesod App]]
* [[Performing Authentication during Testing|Performing Authentication during Testing]]
* [[Flushing Database Between Tests|Flushing-Database-Between-Tests]]

## Template Haskell

* [[Creating string based enums using Template Haskell|Create-String-Based-Enums-With-Template-Haskell]]

## Conduit

* [[Consume a webpage with http-conduit and print progress along the way]]