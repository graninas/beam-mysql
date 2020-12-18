{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}

module Main (main) where

import           Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import           Data.Aeson.Types (Parser)
import           Data.Functor.Identity (Identity)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                Table (PrimaryKey, primaryKey), TableEntity,
                                defaultDbSettings)
import           Database.Beam.MySQL (MySQL, ViaJson (ViaJson), dumpDeleteSQL,
                                      dumpInsertSQL, dumpSelectSQL,
                                      dumpUpdateSQL)
import           Database.Beam.Query (QBaseScope, QExpr, QFieldAssignment,
                                      SqlDelete, SqlInsert, SqlSelect,
                                      SqlUpdate, all_, delete, filter_, insert,
                                      insertValues, select, toNewValue,
                                      updateTable, val_, (==.))
import           GHC.Generics (Generic)
import           Test.Hspec (describe, hspec, it, shouldBe)

-- See "mysql-haskell".Database.MySQL.Protocol.Escape
-- and  <http://dev.mysql.com/doc/refman/5.7/en/string-literals.html>
main :: IO ()
main = hspec $ do
  describe "'\\' in generated SQL for text" $ do
    it "should be escaped in INSERT VALUES literals" $
      dumpInsertSQL insertStmt `shouldBe`
        Just "INSERT INTO `test_table` (text) VALUES ('foo\\\"');"
    it "should be escaped in SELECT WHERE literals" $
      dumpSelectSQL selectWhereStmt `shouldBe`
        Just "SELECT `t0`.`text` AS `res0` FROM `test_table` AS `t0` WHERE (`t0`.`text`) = ('foo\\\"');"
    it "should be escaped in UPDATE WHERE literals" $
      dumpUpdateSQL updateWhereStmt `shouldBe`
        Just "UPDATE `test_table` SET `text`='bar\\\"' WHERE (`text`) = ('foo\\\"');"
    it "should be escaped in DELETE WHERE literals" $
      dumpDeleteSQL deleteWhereStmt `shouldBe`
        Just "DELETE FROM `test_table` WHERE (`text`) = ('foo\\\"');"
  describe "'Bobby Tables'-style tricks in generated SQL via JSON" $ do
    it "should be escaped in INSERT VALUES literals" $
      dumpInsertSQL insertJsonStmt `shouldBe`
        Just "INSERT INTO `json_table` (json) VALUES ('\\\"\\'; DROP TABLE students; --\\\"');"
    it "should be escaped in SELECT WHERE literals" $
      dumpSelectSQL selectWhereJsonStmt `shouldBe`
        Just "SELECT `t0`.`json` AS `res0` FROM `json_table` AS `t0` WHERE (`t0`.`json`) = ('\\\"\\'; DROP TABLE students; --\\\"');"
    it "should be escaped in UPDATE WHERE literals" $
      dumpUpdateSQL updateWhereJsonStmt `shouldBe`
        Just "UPDATE `json_table` SET `json`='\\\"\\'; DROP TABLE students; --\\\"' WHERE (`json`) = ('\\\"\\'; DROP TABLE students; --\\\"');"
    it "should be escaped in DELETE WHERE literals" $
      dumpDeleteSQL deleteWhereJsonStmt `shouldBe`
        Just "DELETE FROM `json_table` WHERE (`json`) = ('\\\"\\'; DROP TABLE students; --\\\"');"

-- Helpers

bobbyTables :: Text
bobbyTables = "\'; DROP TABLE students; --"

data Evil = Evil
  deriving stock (Eq, Show)

instance ToJSON Evil where
  toJSON _ = toJSON bobbyTables

instance FromJSON Evil where
  parseJSON = withText "Evil" go
    where
      go :: Text -> Parser Evil
      go t =
        if t == bobbyTables
        then pure Evil
        else fail "Not possible"

insertStmt :: SqlInsert MySQL TestT
insertStmt =
  insert (_testTestTable testDB) . insertValues $ [
    TestT "foo\""
    ]

insertJsonStmt :: SqlInsert MySQL JsonT
insertJsonStmt =
  insert (_testJsonTable testDB) . insertValues $ [
    JsonT (ViaJson Evil)
    ]

selectWhereStmt :: SqlSelect MySQL (TestT Identity)
selectWhereStmt = select . filter_ go . all_ . _testTestTable $ testDB
  where
    go :: TestT (QExpr MySQL QBaseScope) -> QExpr MySQL QBaseScope Bool
    go row = _testText row ==. val_ "foo\""

selectWhereJsonStmt :: SqlSelect MySQL (JsonT Identity)
selectWhereJsonStmt = select . filter_ go . all_ . _testJsonTable $ testDB
  where
    go :: JsonT (QExpr MySQL QBaseScope) -> QExpr MySQL QBaseScope Bool
    go row = _testJson row ==. val_ (ViaJson Evil)

updateWhereStmt :: SqlUpdate MySQL TestT
updateWhereStmt = updateTable (_testTestTable testDB) upd wher
  where
    upd :: TestT (QFieldAssignment MySQL TestT)
    upd = TestT (toNewValue (val_ "bar\""))
    wher :: forall s . TestT (QExpr MySQL s) -> QExpr MySQL s Bool
    wher row = _testText row ==. val_ "foo\""

updateWhereJsonStmt :: SqlUpdate MySQL JsonT
updateWhereJsonStmt = updateTable (_testJsonTable testDB) upd wher
  where
    upd :: JsonT (QFieldAssignment MySQL JsonT)
    upd = JsonT (toNewValue (val_ (ViaJson Evil)))
    wher :: forall s . JsonT (QExpr MySQL s) -> QExpr MySQL s Bool
    wher row = _testJson row ==. val_ (ViaJson Evil)

deleteWhereStmt :: SqlDelete MySQL TestT
deleteWhereStmt = delete (_testTestTable testDB) go
  where
    go :: (forall s' . TestT (QExpr MySQL s')) -> QExpr MySQL s Bool
    go row = _testText row ==. val_ "foo\""

deleteWhereJsonStmt :: SqlDelete MySQL JsonT
deleteWhereJsonStmt = delete (_testJsonTable testDB) go
  where
    go :: (forall s' . JsonT (QExpr MySQL s')) -> QExpr MySQL s Bool
    go row = _testJson row ==. val_ (ViaJson Evil)

newtype TestT (f :: Type -> Type) = TestT
  {
    _testText :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table TestT where
  data PrimaryKey TestT (f :: Type -> Type) =
    TestTPK (Columnar f Text)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = TestTPK . _testText

newtype JsonT (f :: Type -> Type) = JsonT
  {
    _testJson :: Columnar f (ViaJson Evil)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table JsonT where
  data PrimaryKey JsonT (f :: Type -> Type) =
    JsonTPK (Columnar f (ViaJson Evil))
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = JsonTPK . _testJson

data TestDB (f :: Type -> Type) = TestDB
  {
    _testTestTable :: f (TableEntity TestT),
    _testJsonTable :: f (TableEntity JsonT)
  }
  deriving stock (Generic)
  deriving anyclass (Database MySQL)

testDB :: DatabaseSettings MySQL TestDB
testDB = defaultDbSettings
