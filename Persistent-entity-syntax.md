Persistent's entity syntax has acquired quite a few special cases. The goal of this page is to document all of them. This page is not yet fully up-to-date, please contribute!

## Conversion table (migrations)

Haskell	   |  PostGre SQL         |  MySQL            |  MongoDB      |  SQLLite
-----------|----------------------|-------------------|---------------|---------
Text	   |  VARCHAR             |  TEXT             | String        |  VARCHAR
ByteString |  BYTEA               |  BLOB             | BinData       |  BLOB
Int        |  INT8                |  BIGINT(20)       | NumberLong    |  INTEGER
Double     |  DOUBLE PRECISION    |  DOUBLE           | Double        |  REAL
Rational   |  NUMERIC(22, 12)     |  DECIMAL(32,20    | *Unsupported* |  NUMERIC(32,20)
Bool       |  BOOLEAN             |  TINYINT(1)       | Boolean       |  BOOLEAN
Day        |  DATE                |  DATE             | NumberLong    |  DATE
TimeOfDay  |  TIME                |  TIME             | *Unsupported* |  TIME
UTCTime    |  TIMESTAMP           |  DATETIME         | Date          |  TIMESTAMP
ZonedTime  |  TIMESTAMP+TIMEZONE  |  VARCHAR(50)      | String        |  TIMESTAMP


## Compatibility tables

Haskell type    | Compatible MySQL types
----------------|--------------------------
Bool            | Tiny
Int8            | Tiny
Int16           | Tiny,Short
Int32           | Tiny,Short,Int24,Long
Int             | Tiny,Short,Int24,Long,LongLong\*
Int64           | Tiny,Short,Int24,Long,LongLong
Integer         | Tiny,Short,Int24,Long,LongLong
Word8           | Tiny
Word16          | Tiny,Short
Word32          | Tiny,Short,Int24,Long
Word64          | Tiny,Short,Int24,Long,LongLong
Double          | Float,Double,Decimal,NewDecimal,Tiny,Short,Int24,Long
Ratio Integer   | Float,Double,Decimal,NewDecimal,Tiny,Short,Int24,Long,LongLong
ByteString      | VarChar,TinyBlob,MediumBlob,LongBlob,Blob,VarString,String,Set,Enum
Lazy.ByteString | VarChar,TinyBlob,MediumBlob,LongBlob,Blob,VarString,String,Set,Enum
Encoding.Text\*\* | VarChar,TinyBlob,MediumBlob,LongBlob,Blob,VarString,String,Set,Enum
Lazy.Text       | VarChar,TinyBlob,MediumBlob,LongBlob,Blob,VarString,String,Set,Enum
[Char]/String   | VarChar,TinyBlob,MediumBlob,LongBlob,Blob,VarString,String,Set,Enum
UTCTime         | DateTime,Timestamp
Day             | Year,Date,NewDate
TimeOfDay       | Time

\* When `Word` size is 64bit

\*\* Utf8 only

| Not currently supported
|-----------------------
| Word
| Float
| Scientific [#225](https://github.com/yesodweb/persistent/issues/225)

See [MySQL.Simple.Result](http://hackage.haskell.org/package/mysql-simple/docs/Database-MySQL-Simple-Result.html)

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
Requires `{-# LANGUAGE FlexibleInstances #-}`

## Changing table/collection name

```
Person sql=peoples
    name Text
```

## Change table/collection id field name and/or type (persistent >= 2.1)

```
Person idName=name idtType=Text
   phone Text
```

## Sum types

You'll frequently want to store an enum of values in your database. For example, you might describe a `Person`'s employment status as being `Employed`, `Unemployed`, or `Retired`. In Haskell this is represented with a sum type, and Persistent provides a Template Haskell function to marshall these values to and from the database:

```haskell
-- @Employment.hs
{-# LANGUAGE TemplateHaskell #-}
module Employment where

import Database.Persist.TH
import Prelude

data Employment = Employed | Unemployed | Retired
    deriving (Show, Read, Eq)
derivePersistField "Employment"
```

`derivePersistField` stores sum type values as strings in the database. While not as efficient as using integers, this approach simplifies adding and removing values from your enumeration.

> Due to the GHC Stage Restriction, the call to the Template Haskell function `derivePersistField` must be in a separate module than where the generated code is used

Use the module by importing it into your `Model.hs` file:

```haskell
-- @Model.hs
import Employment
```

and use it in the `models` DSL:

```
Person
    employment Employment
```

You can export the Employment module from Import to use it across your app:

```haskell
-- @Import.hs
import Employment as Import
```

## sqltype=

By default, Persistent maps the Haskell types you specify in the Models DSL to an appropriate SQL type in the database (refer to the [conversion table](#conversion-table-migrations) above for the default mappings). Using the `sqltype=` option, you can  customize the SQL type Persistent uses for your column. Use cases include:

* Interacting with an existing database whose column types don't match Persistent's defaults.
* Taking advantage of a specific SQL type's features
    * e.g. Using an equivalent type that has better space or performance characteristics

To use this setting, add the `sqltype=` option after declaring your field name and type:

```
User
    username Text sqltype=varchar(255)
```

### Nullable fields

By default fields will have `NOT NULL` added. To allow `NULL` values, add `Maybe`.

    string Text Maybe

To add `DEFAULT NULL`

    string Text Maybe default=Nothing

## default=

Persistent supports setting default values on SQL backends like so:

```
created UTCTime default=now()
```

The DEFAULT attribute is set with the exact SQL entered in your `models` file—Persistent performs *no analysis* of default values, and simply passes them onto the DBMS verbatim.

The DEFAULT attribute will affect migrations and raw SQL, but will have no impact on normal Persistent calls. You must still specify all fields, for example, when using an `insert` call.

**Note**: Persistent determines whether or not to migrate a column's default value by comparing the exact string found in your `models` file with the one returned by the database. If a database canonicalizes the SQL `FALSE` from your `models` file to `false` in the database, Persistent will think the default value needs to be migrated and [attempt a migration each time you start your app](https://github.com/yesodweb/persistent/issues/241).

To workaround this, find the exact SQL your DBMS uses for the default value. For example, using postgres:

```
psql database_name # Open postgres

\d+ table_name -- describe the table schema
```

```
...
created       | timestamp without time zone | not null default now()
```

Then use the listed default value SQL inside your `models` file.

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

### Uniqueness

    example Text
    UniqueExample example