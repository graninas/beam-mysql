{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Main (main) where

import           Control.Exception.Safe (bracket)
import           Data.AEq ((~==))
import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity)
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                Table (PrimaryKey, primaryKey), TableEntity,
                                defaultDbSettings, runSelectReturningOne)
import           Database.Beam.MySQL (MySQL, ViaJson (ViaJson), runBeamMySQL)
import           Database.Beam.Query (SqlSelect, all_, select)
import           Database.MySQL.Base (MySQLConn, Query (Query), close, connect,
                                      execute_)
import           Database.MySQL.Temp (MySQLDB, toConnectInfo, withTempDB)
import           GHC.Generics (Generic)
import           Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldSatisfy)

main :: IO ()
main = do
  res <- withTempDB go
  hspec . spec $ res
  where
    go :: MySQLDB -> IO (CorrectT Identity)
    go db = bracket (connect . toConnectInfo $ db)
                    close
                    (\conn -> setUpDB conn >>
                              runCorrectQuery conn)

-- Helpers

setUpDB :: MySQLConn -> IO ()
setUpDB conn = traverse_ (execute_ conn) [
  "create database test;",
  "use test",
  makeCorrect,
  insertCorrect
  ]
  where
    makeCorrect :: Query
    makeCorrect = Query $
      "create table correct_table (" <>
      "id bigint primary key auto_increment, " <>
      "int8_varchar varchar(255) not null, " <>
      "int16_varchar varchar(255) not null, " <>
      "int32_varchar varchar(255) not null, " <>
      "int64_varchar varchar(255) not null, " <>
      "float_varchar varchar(255) not null, " <>
      "double_varchar varchar(255) not null, " <>
      "text_tinyint tinyint not null, " <>
      "text_smallint smallint not null, " <>
      "text_int int not null, " <>
      "text_bigint bigint not null, " <>
      "text_float float not null, " <>
      "int8_float float not null, " <>
      "int16_float float not null, " <>
      "int32_float float not null, " <>
      "int64_float float not null, " <>
      "text_double double not null, " <>
      "int8_double double not null, " <>
      "int16_double double not null, " <>
      "int32_double double not null, " <>
      "int64_double double not null, " <>
      "viajson_binary varbinary(255) not null" <>
      ");"
    insertCorrect :: Query
    insertCorrect = Query $
      "insert into correct_table (" <>
      "id, int8_varchar, int16_varchar, int32_varchar, int64_varchar, " <>
      "float_varchar, double_varchar, " <>
      "text_tinyint, text_smallint, text_int, text_bigint, " <>
      "text_float, int8_float, int16_float, int32_float, int64_float, " <>
      "text_double, int8_double, int16_double, int32_double, int64_double, " <>
      "viajson_binary)" <>
      "values (" <>
      "DEFAULT, '10', '256', '65666', '5000000000', '10.05', '10.0500505005', " <>
      "10, 256, 65666, 5000000000, " <>
      "10.05, 10.05, 10.05, 10.05, 10.05, " <>
      "10.0500505005, 10.0500505005, 10.0500505005, 10.0500505005, 10.0500505005, " <>
      "'[1,2,3,4]');"

runCorrectQuery :: MySQLConn -> IO (CorrectT Identity)
runCorrectQuery conn = do
  res <- runBeamMySQL conn . runSelectReturningOne $ query
  case res of
    Nothing   -> fail "Expected query result, but got nothing."
    Just res' -> pure res'
  where
    query :: SqlSelect MySQL (CorrectT Identity)
    query = select . all_ . _testCorrectTable $ testDB

spec :: CorrectT Identity -> Spec
spec res = do
  describe "Lenient parsing (success)" $ do
    it "should parse Int8 from VARCHAR" $
      _correctInt8Varchar res `shouldBe` 10
    it "should parse Int16 from VARCHAR" $
      _correctInt16Varchar res `shouldBe` 256
    it "should parse Int32 from VARCHAR" $
      _correctInt32Varchar res `shouldBe` 65666
    it "should parse Int64 from VARCHAR" $
      _correctInt64Varchar res `shouldBe` 5000000000
    it "should parse Float from VARCHAR" $
      _correctFloatVarchar res `shouldSatisfy` (~== 10.05)
    it "should parse Double from VARCHAR" $
      _correctDoubleVarchar res `shouldSatisfy` (~== 10.0500505005)
    it "should parse Text from TINYINT" $
      _correctTextTinyint res `shouldBe` "10"
    it "should parse Text from SMALLINT" $
      _correctTextSmallint res `shouldBe` "256"
    it "should parse Text from INT" $
      _correctTextInt res `shouldBe` "65666"
    it "should parse Text from BIGINT" $
      _correctTextBigint res `shouldBe` "5000000000"
    it "should parse Text from FLOAT" $
      _correctTextFloat res `shouldBe` "10.05"
    it "should parse Int8 from FLOAT" $
      _correctInt8Float res `shouldBe` 10
    it "should parse Int16 from FLOAT" $
      _correctInt16Float res `shouldBe` 10
    it "should parse Int32 from FLOAT" $
      _correctInt32Float res `shouldBe` 10
    it "should parse Int64 from FLOAT" $
      _correctInt64Float res `shouldBe` 10
    it "should parse Text from DOUBLE" $
      _correctTextDouble res `shouldBe` "10.0500505005"
    it "should parse Int8 from DOUBLE" $
      _correctInt8Double res `shouldBe` 10
    it "should parse Int16 from DOUBLE" $
      _correctInt16Double res `shouldBe` 10
    it "should parse Int32 from DOUBLE" $
      _correctInt32Double res `shouldBe` 10
    it "should parse Int64 from DOUBLE" $
      _correctInt64Double res `shouldBe` 10
    it "should parse ViaJSON from BINARY" $
      _correctViajsonBinary res `shouldBe` ViaJson (V.fromList [1,2,3,4])

data CorrectT (f :: Type -> Type) = CorrectT
  {
    _correctId            :: Columnar f Int64,
    _correctInt8Varchar   :: Columnar f Int8,
    _correctInt16Varchar  :: Columnar f Int16,
    _correctInt32Varchar  :: Columnar f Int32,
    _correctInt64Varchar  :: Columnar f Int64,
    _correctFloatVarchar  :: Columnar f Float,
    _correctDoubleVarchar :: Columnar f Double,
    _correctTextTinyint   :: Columnar f Text,
    _correctTextSmallint  :: Columnar f Text,
    _correctTextInt       :: Columnar f Text,
    _correctTextBigint    :: Columnar f Text,
    _correctTextFloat     :: Columnar f Text,
    _correctInt8Float     :: Columnar f Int8,
    _correctInt16Float    :: Columnar f Int16,
    _correctInt32Float    :: Columnar f Int32,
    _correctInt64Float    :: Columnar f Int64,
    _correctTextDouble    :: Columnar f Text,
    _correctInt8Double    :: Columnar f Int8,
    _correctInt16Double   :: Columnar f Int16,
    _correctInt32Double   :: Columnar f Int32,
    _correctInt64Double   :: Columnar f Int64,
    _correctViajsonBinary :: Columnar f (ViaJson (Vector Int))
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table CorrectT where
  data PrimaryKey CorrectT (f :: Type -> Type) =
    CorrectTPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = CorrectTPK . _correctId

newtype TestDB (f :: Type -> Type) = TestDB
  {
    _testCorrectTable :: f (TableEntity CorrectT)
  }
  deriving stock (Generic)
  deriving anyclass (Database MySQL)

testDB :: DatabaseSettings MySQL TestDB
testDB = defaultDbSettings
