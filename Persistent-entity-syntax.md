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

## Changing table/collection name

```
Person sql=peoples
    name Text
```

## Sum types

FIXME

## sqltype=

FIXME

### Nullable fields

By default fields will have `NOT NULL` added. To allow `NULL` values, add `Maybe`.

    string Text Maybe


## default=

FIXME

Note: if you get migration warnings every time, then you need to tweak defaults. For example, when I had a model definition like so:

    created Day default=CURRENT_TIME
    avsrelax Bool default='false'

every time yesod started I got:

    Migrating: ALTER TABLE "accounts" ALTER COLUMN "created" SET DEFAULT CURRENT_DATE
    Migrating: ALTER TABLE "accounts" ALTER COLUMN "avsrelax" SET DEFAULT 'false'

By building a custom persistent-postgresql with this patch (plus the obvious import)

    --- Database/Persist/Postgresql.hs.orig	2013-06-23 18:40:53.000000000 +0100
    +++ Database/Persist/Postgresql.hs	2014-01-07 11:37:30.387071854 +0000
    @@ -490,6 +492,9 @@
                                 _ -> []
                     modType = if sqltype == sqltype' then [] else [(name, Type sqltype)]
                     modDef =
    +                  let msg = "def is: " ++ show def ++ " and def' is: " ++ show def'
    +                  in trace msg $
                         if def == def'
                             then []
                             else case def of

I found that the correct syntax was:

    created Day default=('now'::text)::date
    avsrelax Bool default=false


## MigrationOnly

Introduced with `persistent-template` 1.2.0. The purpose of this attribute is to mark a field which will be entirely ignored by the normal processing, but retained in the database definition for purposes of migration. This means, in SQL, a column will not be flagged for removal by the migration scripts, even though it is not used in your code. This is useful for phasing out usage of a column before entirely removing it, or having columns which are needed by other tools but not by Persistent.

```
Person
    name Text
    age Int
    unusedField ByteString Maybe MigrationOnly
```

Note that you almost certainly want to either mark the field as `Maybe` or provide a default value, otherwise insertions will fail.

## SafeToRemove

This is intended to be used as part of a deprecation of a field, after `MigrationOnly` has been used usually. This works somewhat as a superset of the functionality of MigrationOnly. In addition, the field will be removed from the database if it is present. Note that this is a destructive change which you are marking as safe.

## Constraints

Migration will remove any manual constraints from your tables. Exception: constraints whose names begin with the string `__manual_` (which starts with two underscores) will be preserved.