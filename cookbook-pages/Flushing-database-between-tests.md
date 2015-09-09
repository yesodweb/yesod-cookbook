Quite common, you want to keep your tests maximally independent, so you want to flush database on each test. Currently there doesn't seem to be way to do this out of the box.

Two possible (reasonable) implementations are:

1. Completely re-create database at each test.
2. Start test in transaction, when test is over rollback transaction.

Second way seems much faster and less hardcode, but I didn't yet manage to implement it (I'm not sure about transaction support for yesod), so I'll describe first way.

NOTE! This requires two patches to be accepted ([0], [1]), one in HSpec, another one in yesod-test. I'll send them today.

In your `tests/main.hs`, modify your test runner to look something like this (assuming your project is called `WebShelf`, which probably isn't true, so please replace that with your db name):

```haskell
-- ... imports
import Test.HSpec
import Yesod.Test

main :: IO ()
main = do
    hspec $ (before removeDb) $ do
        yesodSpecWithSiteGenerator getSiteAction $ do
            homeSpecs
            entriesSpecs
            modelSpecs
            -- ... more specs here

getSiteAction :: IO App
getSiteAction = do
    conf <- Yesod.Default.Config.loadConfig $ (configSettings Testing)
                { csParseExtra = parseExtra
                }
    foundation <- makeFoundation conf
    return foundation

removeDb :: IO ()
removeDb = do
  -- path <- canonicalizePath "WebShelf_test.sqlite3"
  let path = "WebShelf_test.sqlite3"
  isExist <- doesFileExist path
  when isExist (recreateDbWhenExists path)

recreateDbWhenExists :: FilePath -> IO ()
recreateDbWhenExists path = do
  putStrLn $ "Path is: " ++ path
  removeFile "WebShelf_test.sqlite3"
  runSqlite "WebShelf_test.sqlite3" $ do
    (runMigration migrateAll)
  return ()
```

[0]: https://github.com/hspec/hspec/pull/132
[1]: https://github.com/yesodweb/yesod/pull/577