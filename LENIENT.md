# Introduction

Sometimes, additional leniency is desirable when the form of a database schema
may not always be 100% uniform with the 'database types' required by `beam`.
This can be due to schematic drift, or cases where a type for a column doesn't
fully structure or describe the data held there (such as with SQLite). 

By default, it is better to be 'strict' with regard to such cases - namely,
erroring when we encounter data in a form we don't expect. However, for cases
like the ones above, we provide a 'lenient mode' which attempts some semantic
conversions from the data found in the database to the Haskell values based on
the expectations defined by the 'database type' required by `beam`. This can be
enabled by building with the `lenient` flag set. This flag is off by default;
thus, this is opt-in behaviour.

# Description

Consider the 'database type' below:

```haskell
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

import Data.Kind (Type)
import Data.Text (Text)
import Data.Int (Int64)
import Database.Beam (Beamable, Columnar)
import GHC.Generics (Generic)

data TestT (f :: Type -> Type) = TestT {
  testId :: Columnar f Int64,
  testData :: Columnar f (Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)
```

A (possible) SQL schema for the table whose rows `TestT` is meant to represent
is: 

```sql
CREATE TABLE test (
  id BIGINT NOT NULL PRIMARY KEY,
  data VARCHAR(255)
  );
```

We say that `testId` is the _corresponding field_ to `id`, and `id` is the
_corresponding column_ to `testId`. We say that the _demanded type_ of `id` is
`Int64`. We extend this to all such field-column pairings induced by 'database
types' based on SQL tables. For any SQL type `T` and Haskell type `U`, we say
that `U` is _demandable_ from `T` if it is valid to have the demanded type of a
column of type `T` be `U`.

The following table summarizes demandable types for key SQL types, both in
'strict' mode (without the `lenient` flag) and in 'lenient' mode (with the
`lenient` flag). For more information, see subsequent subsections. Anything
listed as demandable in 'strict' mode is also available in 'lenient' mode.

|**SQL Type**     |**Strict demandable type(s)**|**Lenient demandable type(s)**                     |
|-----------------|-----------------------------|---------------------------------------------------|
|``VARCHAR``      |``Text``, ``ByteString``     |``Int{8,16,32,64}``, ``Int``, ``Float``, ``Double``|
|``STRING``       |``Text``, ``ByteString``     |``Int{8,16,32,64}``, ``Int``, ``Float``, ``Double``|                  
|``VARSTRING``    |``Text``, ``ByteString``     |``Int{8,16,32,64}``, ``Int``, ``Float``, ``Double``|
|``TINYBLOB``     |``ByteString``               |                                                   |
|``MEDIUMBLOB``   |``ByteString``               |                                                   | 
|``LONGBLOB``     |``ByteString``               |                                                   |
|``BLOB``         |``ByteString``               |                                                   |
|``ENUM``         |``Text``, ``ByteString``     |``Int{8,16,32,64}``, ``Int``, ``Float``, ``Double``|
|``SET``          |``Text``, ``ByteString``     |``Int{8,16,32,64}``, ``Int``, ``Float``, ``Double``|
|-----------------|----------------------------------------------------------|-------------------------------|
|``TINYINT``      |``Int{8,16,32,64}``, ``Int``, ``Rational``, ``Scientific``|``Text``|
|``SMALLINT``     |``Int{16,32,64}``, ``Int``, ``Rational``, ``Scientific``  |``Text``|
|``MEDIUMINT``    |``Int{16,32,64}``, ``Int``, ``Rational``, ``Scientific``  |``Text``|
|``INT``          |``Int{32,64}``, ``Int``, ``Rational``, ``Scientific``     |``Text``|
|``BIGINT``       |``Int64``, ``Int``, ``Rational``, ``Scientific``          |``Text``|
|-----------------|---------------------|--------|
|``FLOAT``        |``Float``, ``Double``|``Text, Int8, Int16, Int32, Int64``|
|``DOUBLE``       |``Double``           |``Text, Int8, Int16, Int32, Int64``|

## Textual types

In 'strict' mode, ``Text`` and ``Bytestring`` are both demandable from anything
'textual' in SQL. In 'lenient' mode, The following are also demandable:

* ``Int`` and any fixed-width ``Int`` types, if the value is a textual
  representation of a signed integer which fits into the representation.
* ``Float`` and ``Double``, if the value is a textual representation of an
  IEEE-754 single or double-precision floating-point value respectively.

## Signed fixed-width integral types

In 'strict' mode, fixed-with signed integral types are demandable from any SQL
fixed-width signed integral type, provided the value 'fits'. ``Int`` is
demandable under the same conditions as fixed-width integral types. ``Rational``
and ``Scientific`` are always demandable. In 'lenient' mode, the following are
also demandable:

* ``Text``, as the value's textual representation in base-10.

## Fixed-width floating-point types

In 'strict' mode, only ``Double`` is demandable from ``DOUBLE``, and both
``Float`` and ``Double`` are demandable from ``Float``. In 'lenient' mode, the
following are also demandable: 

* ``Text``, as the IEEE-754 textual approximation of the value.
* Fixed-width signed integral types, as a truncation of the value, provided it
  'fits' into the representation.

## Via intermediate JSON

In 'strict' mode, only textual types are considered as valid for such
derivations. In 'lenient' mode, binary types are also permitted.
