 ---
 Example with Yesod Scaffold and Your Own Model:
 ---

1.  Your object representing clients must provide a unique identifier field, a
    field for the password hash, and a field for the password salt. Example:

    ```haskell
    -- in config/models

    Person
        email Text
        passwd Text
        salt Text
        UniqueEmail email 
    ```

    In this example, 'email' will be the unique identifier.

2.  You must provide

    ```haskell
    instance HashDBUser (<yourObject>Generic backend)
    ```

    in the module that shares your model. In the scaffolded site, that module
    is Model. Continuing the Person example,

    ```haskell
    -- in Model.hs
instance HashDBUser Person where
    userPasswordHash = Just . personPassword
    userPasswordSalt = Just . personSalt
    setSaltAndPasswordHash s h p = p { personSalt     = s
                                 , personPassword = h
                                 }

    ```

3. Now, in Foundation.hs, we hook Auth.HashDB into your foundation. You must
   add an import of Yesod.Auth.HashDB, of course, and then modify the YesodAuth
   instance like so:

    ```haskell
    -- in Foundation.hs
    instance YesodAuth <MySite> where
        type AuthId <MySite> = PersonId
        -- ... loginDest, etc.
        getAuthId creds = getAuthIdHashDB AuthR (Just . UniqueEmail) creds
        authPlugins = [authHashDB (Just . UniqueEmail)]
    ```