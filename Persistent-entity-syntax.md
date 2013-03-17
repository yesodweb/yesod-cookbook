Persistent's entity syntax has acquired quite a few special cases. The goal of this page is to document all of them. This page is not yet fully up-to-date, please contribute!

## MigrationOnly

Introduced with `persistent-template` 1.1.3. The purpose of this attribute is to mark a field which will be entirely ignored by the normal processing, but retained in the database definition for purposes of migration. This means, in SQL, a column will not be flagged for removal by the migration scripts, even though it is not used in your code. This is useful for phasing out usage of a column before entirely removing it, or having columns which are needed by other tools but not by Persistent.

```
Person
    name Text
    age Int
    unusedField ByteString Maybe MigrationOnly
```