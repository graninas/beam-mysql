# Changelog

## 1.2.1.0

* Added a wrapper `ViaJson`, plus instances, to aid unwrapping values packed
  into JSON. 
* Added a wrapper `FakeUTC`, plus instances, to assist with use of `UTCTime` in
  cases where you know that the database is zoned to UTC.
* Fix some rendering issues.
* Ensure that `runInsertRowReturning` throws an exception if asked to work over
  a table without a primary key.
* More tests.

## 1.2.0.0

* Removed `Aeson.Value`, `NominalDiffTime`, `Rational` instances of `FromField`
* Added support for a lenient mode (see LENIENT.md)
* Reintroduce `runInsertRowReturning`
* Improve error messages
* Rewrite benchmarks using Criterion
* Ensure proper escaping of `Text` values
* Add `dumpInsertSQL`, `dumpSelectSQL`, `dumpDeleteSQL`, `dumpUpdateSQL` for 
  debugging SQL query generator output
* Restructure internal SQL AST representation
* Many more tests

## 1.0.0.0

* Use `mysql-haskell` instead of `mysql`
* `Aeson.Value` instance of `FromField` more efficient and safer
* `NominalDiffTime` instance of `FromField` now handles small time differences
  properly
* Connection type no longer exported.
* `FLOAT` can only be loaded as a `Float`; analogous for `DOUBLE` and `Double`.
* Only `Text` and `ByteString` can now be loaded (not lazy, not `Char8`).
* Many unsafe conversions have been disabled.
* `TimeOfDay` and `NominalDiffTime` can now be loaded correctly.
* `OFFSET` without `LIMIT` now does nothing when requested in a query.
* `FromField` no longer requires `IO`.
* Remove URI-based connections
* Remove `runInsertRowReturning` (should be unnecessary)
