A question which [comes up quite a bit](https://groups.google.com/forum/#%21topic/yesodweb/rg8Ujhc-d0w) is why persistent-postgresql stores UTCTime values in the database as `TIMESTAMP WITHOUT TIME ZONE` instead of `WITH TIME ZONE`. There are two datatypes in Haskell, and two in PostgreSQL, relevant to this discussion:

* Haskell
    * UTCTime: An exact moment in time, in the UTC timezone.
    * ZonedTime: A UTCTime plus a timezone.
* PostgreSQL
    * WITHOUT TIME ZONE: A time stamp, which could be in any time zone.
    * WITH TIME ZONE: An exact moment in time, with a given time zone.

It's clear from this that `ZonedTime` and `WITH TIME ZONE` line up perfectly. The problem is that there's a small mismatch between UTCTime and `WITHOUT`. The latter could imply *any* timezone, which is quite problematic. When Persistent uses this SQL type, it guarantees that all stored timestamps are in UTC time, and therefore does not run into any issues of mismatched timezones.

However, why not get the extra safety of being explicit about the timezone selection in the database? The problem comes down to what Persistent should do in the case of a value in a non-UTC timezone. There are essentially two options:

1. Raise an exception about unexpected data. This is suboptimal, since we would like to avoid runtime errors like this whenever possible, and instead rely on the type system + SQL schema to keep things safe.
2. Simply discard the timezone information. The problem here is that roundtripping will no longer work: reading a value, dropping the timezone, and writing back to the database would force a conversion to the UTC timezone, which may not be what a user wants.

So in your application, what should you do? Here's my recommendation:

* If your database is only being accessed from Persistent, using `UTCTime` and `WITHOUT TIME ZONE` is perfectly safe, and is a simpler option.
    * Unless, of course, you actually want to store timezone information, in which case you should use `ZonedTime`!
* If you're dealing with an existing database, or publishing data that other applications will need to see, always use `ZonedTime`. Inside your application, you can trivially converted to a `UTCTime` instead. But this will force you to be aware of any data loss you may be performing, and of any roundtripping issues you may be introducing to your database.