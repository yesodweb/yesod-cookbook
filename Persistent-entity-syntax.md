Persistent's entity syntax has acquired quite a few special cases. The goal of this page is to document all of them. This page is not yet fully up-to-date, please contribute!

## Deriving

Persistent automatically derives some typeclasses, but the typeclasses derived can be changed.

```
Person
    name Text
    deriving Show
```

## JSON instances

You can automatically get ToJSON and FromJSON instances for any entity by adding `json` to the entity line:

```
Person json
    name Text
```

## Sum types

FIXME

## MigrationOnly

_Sorry, this doesn't work right now..._

Introduced with `persistent-template` 1.1.3. The purpose of this attribute is to mark a field which will be entirely ignored by the normal processing, but retained in the database definition for purposes of migration. This means, in SQL, a column will not be flagged for removal by the migration scripts, even though it is not used in your code. This is useful for phasing out usage of a column before entirely removing it, or having columns which are needed by other tools but not by Persistent.

```
Person
    name Text
    age Int
    unusedField ByteString Maybe MigrationOnly
```
