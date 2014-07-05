Persistent's entity syntax has acquired quite a few special cases. The goal of this page is to document all of them. This page is not yet fully up-to-date, please contribute!

## Conversion table (migrations)

Haskell	   |  PostGre SQL         |  MySQL            |  MongoDB  |  SQLLite
-----------|----------------------|-------------------|-----------|---------
Text	   |  VARCHAR             |  TEXT             | FIXME     |  FIXME
ByteString |  BYTEA               |  BLOB             | FIXME     |  FIXME
Int        |  INT8                |  BIGINT(20)       | FIXME     |  FIXME
Double     |  DOUBLE PRECISION    |  DOUBLE           | FIXME     |  FIXME
Rational   |  NUMERIC(22, 12)     |  DECIMAL(32,20    | FIXME     |  FIXME
Bool       |  BOOLEAN             |  TINYINT(1)       | FIXME     |  FIXME
Day        |  DATE                |  DATE             | FIXME     |  FIXME
TimeOfDay  |  TIME                |  TIME             | FIXME     |  FIXME
UTCTime    |  TIMESTAMP           |  DATETIME         | FIXME     |  FIXME
ZonedTime  |  TIMESTAMP+TIMEZONE  |  VARCHAR(50)      | FIXME     |  FIXME


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

## Sum types

FIXME

## sqltype=

FIXME

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

This is intended to be used as part of a deprecation of a field, after `MigrationOnly` has been used usually. This works somewhat as a superset omyf the functionality of MigrationOnly. In addition, the field will be removed from the database if it is present. Note that this is a destructive change which you are marking as safe.

## Constraints

Migration will remove any manual constraints from your tables. Exception: constraints whose names begin with the string `__manual_` (which starts with two underscores) will be preserved.

### Uniqueness

    example Text
    UniqueExample example

