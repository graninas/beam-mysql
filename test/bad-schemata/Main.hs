{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Main (main) where

import           Control.Exception.Safe (bracket, catch)
import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity)
import qualified Data.HashSet as HS
import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                SqlSelect, Table (PrimaryKey, primaryKey),
                                TableEntity, all_, defaultDbSettings,
                                runSelectReturningOne, select)
import           Database.Beam.Backend.SQL.Row (FromBackendRow)
import           Database.Beam.MySQL (ColumnDecodeError (Can'tDecodeIntoDemanded),
                                      MySQL, columnIndex, demandedType,
                                      runBeamMySQL, sqlType, tablesInvolved,
                                      value)
import           Database.MySQL.Base (MySQLConn, Query (Query), close, connect,
                                      execute_)
import           Database.MySQL.Temp (MySQLDB, toConnectInfo, withTempDB)
import           GHC.Generics (Generic)
import           Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = do
  res <- withTempDB go
  hspec . spec $ res
  where
    go :: MySQLDB -> IO (ColumnDecodeError, ColumnDecodeError)
    go db = bracket (connect . toConnectInfo $ db)
                    close
                    (\conn -> setUpDB conn >>
                              runQueryCatching conn)
    runQueryCatching :: MySQLConn -> IO (ColumnDecodeError, ColumnDecodeError)
    runQueryCatching conn = do
      res1 <- catch (runDBDumping conn query) catcher
      res2 <- catch (runDBDumping conn bigQuery) catcher
      pure (res1, res2)
    catcher :: ColumnDecodeError -> IO ColumnDecodeError
    catcher = pure
    runDBDumping :: (Show t, FromBackendRow MySQL t) =>
      MySQLConn -> SqlSelect MySQL t -> IO a
    runDBDumping conn q = do
      res <- runBeamMySQL conn . runSelectReturningOne $ q
      fail . ("Query ran, but should have thrown. " <>) $ case res of
        Nothing  -> "No result."
        Just row -> "Result: " <> show row
    query :: SqlSelect MySQL (TestT Identity)
    query = select . all_ . _testTestTable $ testDB
    bigQuery :: SqlSelect MySQL (BigTestT Identity)
    bigQuery = select . all_ . _testBigtestTable $ testDB

-- Helpers

spec :: (ColumnDecodeError, ColumnDecodeError) -> Spec
spec (errSmall, errBig) = do
  describe "Errors on mistyped fields (simple)" $ do
    it "should contain the name of the table" $
      (HS.member "test_table" . tablesInvolved $ errSmall) `shouldBe` True
    it "should indicate the right column" $
      columnIndex errSmall `shouldBe` 1
    it "should say we have a type mismatch" $
      isCan'tDecode errSmall `shouldBe` True
    it "should state the expected type correctly" $
      demandedType errSmall `shouldBe` "Text"
    it "should state the actual (SQL) type correctly" $
      sqlType errSmall `shouldBe` "MySQL LongLong"
    it "should report the value found" $
      value errSmall `shouldBe` "MySQLInt64 2"
  describe "Errors on mistyped fields (complex)" $ do
    it "should contain the name of the table" $
      (HS.member "bigtest_table" . tablesInvolved $ errBig) `shouldBe` True
    it "should indicate the right column" $
      columnIndex errBig `shouldBe` 2
    it "should say we have a type mismatch" $
      isCan'tDecode errBig `shouldBe` True
    it "should state the expected type correctly" $
      demandedType errBig `shouldBe` "Int64"
    it "should state the actual (SQL) type correctly" $
      sqlType errBig `shouldBe` "MySQL VarString"
    it "should report the value found" $
      value errBig `shouldBe` "MySQLText \"bar\""
  where
    isCan'tDecode :: ColumnDecodeError -> Bool
    isCan'tDecode = \case
      Can'tDecodeIntoDemanded{} -> True
      _ -> False

data TestT (f :: Type -> Type) = TestT
  {
    _testId   :: Columnar f Int64,
    _testText :: Columnar f (Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table TestT where
  data PrimaryKey TestT (f :: Type -> Type) =
    TestTPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = TestTPK . _testId

deriving stock instance Show (TestT Identity)

data BigTestT (f :: Type -> Type) = BigTestT
  {
    _bigtestId     :: Columnar f Text,
    _bigtestField1 :: Columnar f Bool,
    _bigtestField2 :: Columnar f Int64, -- problem is here
    _bigtestField3 :: Columnar f (Maybe Text),
    _bigtestField4 :: Columnar f (Maybe Int64)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table BigTestT where
  data PrimaryKey BigTestT (f :: Type -> Type) =
    BigTestTPK (Columnar f Text)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = BigTestTPK . _bigtestId

deriving stock instance Show (BigTestT Identity)

data TestDB (f :: Type -> Type) = TestDB
  {
    _testTestTable    :: f (TableEntity TestT),
    _testBigtestTable :: f (TableEntity BigTestT)
  }
  deriving stock (Generic)
  deriving anyclass (Database MySQL)

testDB :: DatabaseSettings MySQL TestDB
testDB = defaultDbSettings

-- DO NOT use this outside of bracket!
setUpDB :: MySQLConn -> IO ()
setUpDB conn = traverse_ (execute_ conn) [
  "create database test;",
  "use test",
  makeTest,
  insertTest,
  makeBigTest,
  insertBigTest
  ]
  where
    makeTest :: Query
    makeTest = Query $
      "create table test_table (" <>
      "id bigint primary key, " <>
      "text bigint" <>
      ");"
    makeBigTest :: Query
    makeBigTest = Query $
      "create table bigtest_table (" <>
      "id varchar(255) primary key, " <>
      "field1 bit(1) not null, " <>
      "field2 varchar(255) not null, " <>
      "field3 varchar(255), " <>
      "field4 bigint" <>
      ");"
    insertTest :: Query
    insertTest = Query $
      "insert into test_table (id, text) " <>
      "values (1, 2);"
    insertBigTest :: Query
    insertBigTest = Query $
      "insert into bigtest_table (id, field1, field2, field3, field4) " <>
      "values ('foo', 0, 'bar', NULL, 10);"
