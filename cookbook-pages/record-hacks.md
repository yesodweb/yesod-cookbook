Basic type class

```haskell
class EmailString a where
       email :: a -> String
       setEmail :: a -> String -> a
```

Functional Dependencies

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

data User = User { userEmail :: String } deriving Show
data Account = Account { accountEmail :: String } deriving Show

class EmailString record t | record -> t where
    email :: record -> t

instance EmailString User String where
    email = userEmail

instance EmailString Account String where
    email = accountEmail
```

Type Families

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

module FieldExample where

data Record1 = Record1 { _record1Field1 :: Int }
data Record2 = Record2 { _record2Field1 :: String }
data Record3 = Record3 { _record3Field1 :: Int }
type family RecordField1Type a
type instance RecordField1Type Record1 = Int
type instance RecordField1Type Record2 = String
type instance RecordField1Type Record3 = Int

class RecordField1 a where
  field1 :: a -> RecordField1Type a
instance RecordField1 Record1 where
  field1 = _record1Field1
instance RecordField1 Record2 where
  field1 = _record2Field1
instance RecordField1 Record3 where
  field1 = _record3Field1

getFieldFromRecord1 :: Record1 -> Int
getFieldFromRecord2 :: Record2 -> String
getFieldFromRecord3 :: Record3 -> Int
getFieldFromRecord1 = field1
getFieldFromRecord2 = field1
getFieldFromRecord3 = field1
```

Associated Type Families

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
module FieldExample where
data Record1 = Record1 { _record1Field1 :: Int }
data Record2 = Record2 { _record2Field1 :: String }
data Record3 = Record3 { _record3Field1 :: Int }

class RecordField1 a where
  type RecordField1Type a
  field1 :: a -> RecordField1Type a

instance RecordField1 Record1 where
  type RecordField1Type Record1 = Int 
  field1 = _record1Field1
instance RecordField1 Record2 where
  type RecordField1Type Record2 = String
  field1 = _record2Field1
instance RecordField1 Record3 where
  type RecordField1Type Record3 = Int 
  field1 = _record3Field1

getFieldFromRecord1 :: Record1 -> Int 
getFieldFromRecord2 :: Record2 -> String
getFieldFromRecord3 :: Record3 -> Int 
getFieldFromRecord1 = field1
getFieldFromRecord2 = field1
getFieldFromRecord3 = field1
```