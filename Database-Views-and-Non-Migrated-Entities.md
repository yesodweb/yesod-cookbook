As Persistent currently has no concept of database views, to use database views with Yesod, you will have to create a separate entities definition file, define the views in this file, and tell Yesod not to run migrations on this file.

For example, the Yesod 1.2 scaffolder by default creates a normal entities defintion file at config/models, so you may wish to put your separate file alongside this one, as an example creating **config/modelsNoMigration**.

Still within the context of the Yesod 1.2 scaffolder, then open Model.hs in the root directory of the app, which by default should appear as follows:
```haskell
module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
```
Model.hs pulls in your entity definitions from config/models and any addition custom file you create, so in order to make Yesod aware of your new file, append the following to Model.hs:
```haskell
share [mkPersist sqlOnlySettings]
    $(persistFileWith lowerCaseSettings "config/modelsNoMigration")
```
This way migrations will not be run on your database view definitions.