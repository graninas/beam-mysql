{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}

module Main (main) where

import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                Table (PrimaryKey, primaryKey), TableEntity,
                                defaultDbSettings)
import           Database.Beam.MySQL (MySQL, dumpInsertSQL)
import           Database.Beam.Query (QExpr, SqlInsert, default_, insert,
                                      insertExpressions, val_)
import           GHC.Generics (Generic)
import           Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Values inserts" $ do
    it "should generate a single-value insert correctly" $
      dumpInsertSQL simpleInsert `shouldBe`
        Just "INSERT INTO `test_table` (id, data) VALUES (DEFAULT, 'foo');"
    it "should generate a multi-value insert correctly" $
      dumpInsertSQL multiInsert `shouldBe`
        Just "INSERT INTO `test_table` (id, data) VALUES (DEFAULT, 'foo'), (DEFAULT, 'bar');"

-- Helpers

-- Beam boilerplate

data TestT (f :: Type -> Type) = TestT
  { _testId   :: Columnar f Int64,
    _testData :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table TestT where
  data PrimaryKey TestT (f :: Type -> Type) =
    TestTPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = TestTPK . _testId

newtype TestDB (f :: Type -> Type) = TestDB
  {
    _testTestTable :: f (TableEntity TestT)
  }
  deriving stock (Generic)
  deriving anyclass (Database MySQL)

testDB :: DatabaseSettings MySQL TestDB
testDB = defaultDbSettings

-- Inserts

simpleInsert :: SqlInsert MySQL TestT
simpleInsert =
  insert (_testTestTable testDB) (insertExpressions [go])
  where
    go :: forall s . TestT (QExpr MySQL s)
    go = TestT default_ (val_ "foo")

multiInsert :: SqlInsert MySQL TestT
multiInsert =
  insert (_testTestTable testDB) (insertExpressions go)
  where
    go :: forall s . [TestT (QExpr MySQL s)]
    go = [
      TestT default_ (val_ "foo"),
      TestT default_ (val_ "bar")
      ]
