{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main (main) where

import           Control.Exception.Safe (bracket)
import           Control.Monad (replicateM_)
import           Data.Aeson (Value (..))
import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity)
import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (take)
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                SqlInsert, Table (PrimaryKey, primaryKey),
                                TableEntity, defaultDbSettings, default_,
                                insert, insertExpressions, val_)
import           Database.Beam.MySQL (MySQL, ViaJson (ViaJson), runBeamMySQL,
                                      runInsertRowReturning)
import           Database.MySQL.Base (MySQLConn, Query (Query), close, connect,
                                      execute_)
import           Database.MySQL.Temp (toConnectInfo, withTempDB)
import           GHC.Generics (Generic)
import           Prelude hiding (take)
import           Test.Hspec (beforeAll, describe, hspec, it, shouldBe)
import           Test.QuickCheck (Gen, arbitrary, generate, liftArbitrary,
                                  oneof, scale)
import           Test.QuickCheck.Instances.Text ()
import           Test.QuickCheck.Instances.UnorderedContainers ()
import           Test.QuickCheck.Instances.Vector ()

main :: IO ()
main = withTempDB (\db -> bracket (connect . toConnectInfo $ db)
                                  close
                                  (\conn -> setUpDB conn >> spec conn))

-- Helpers

setUpDB :: MySQLConn -> IO ()
setUpDB conn = traverse_ (execute_ conn) [
  "create database test;",
  "use test",
  makeTest
  ]
  where
    makeTest :: Query
    makeTest = Query $
      "create table test_table (" <>
      "id int not null primary key auto_increment, " <>
      "from_bool varchar(255) not null, " <>
      "from_double varchar(255) not null, " <>
      "from_string varchar(255) not null, " <>
      "from_array varchar(255) not null, " <>
      "from_object varchar(255) not null" <>
      ");"

spec :: MySQLConn -> IO ()
spec conn = hspec $ do
  beforeAll go $ do
    describe "ViaJson" . replicateM_ 100 $
      it "should give back what we put in" $
        \(actual, b, d, s, arr, obj) -> do
          _testFromBool <$> actual `shouldBe` Just b
          _testFromDouble <$> actual `shouldBe` Just d
          _testFromString <$> actual `shouldBe` Just s
          _testFromArray <$> actual `shouldBe` Just arr
          _testFromObject <$> actual `shouldBe` Just obj
  where
    go :: IO (Maybe (TestT Identity),
                      ViaJson Value,
                      ViaJson Value,
                      ViaJson Value,
                      ViaJson Value,
                      ViaJson Value)
    go = do
      b <- generate (ViaJson <$> genJsonBool)
      d <- generate (ViaJson <$> genJsonNumber)
      s <- generate (ViaJson <$> genJsonString)
      arr <- generate (ViaJson <$> genJsonArray genJsonBool)
      obj <- generate (ViaJson <$> genJsonObject)
      res <- runBeamMySQL conn . runInsertRowReturning . ins b d s arr $ obj
      pure (res, b, d, s, arr, obj)
    ins ::
      ViaJson Value ->
      ViaJson Value ->
      ViaJson Value ->
      ViaJson Value ->
      ViaJson Value ->
      SqlInsert MySQL TestT
    ins b d s arr obj = insert (_testTestTable testDB) (insertExpressions [
      TestT default_ (val_ b) (val_ d) (val_ s) (val_ arr) (val_ obj)
      ])

-- Beam stuff

data TestT (f :: Type -> Type) = TestT
  { _testId         :: Columnar f Int64,
    _testFromBool   :: Columnar f (ViaJson Value),
    _testFromDouble :: Columnar f (ViaJson Value),
    _testFromString :: Columnar f (ViaJson Value),
    _testFromArray  :: Columnar f (ViaJson Value),
    _testFromObject :: Columnar f (ViaJson Value)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table TestT where
  data PrimaryKey TestT (f :: Type -> Type) =
    TestTPK (Columnar f Int64)
      deriving stock (Generic)
      deriving anyclass (Beamable)
  primaryKey = TestTPK . _testId

deriving stock instance Eq (TestT Identity)

deriving stock instance Show (TestT Identity)

newtype TestDB (f :: Type -> Type) = TestDB
  {
    _testTestTable :: f (TableEntity TestT)
  }
  deriving stock (Generic)
  deriving anyclass (Database MySQL)

testDB :: DatabaseSettings MySQL TestDB
testDB = defaultDbSettings

-- Arbitrary stuff

genJsonBool :: Gen Value
genJsonBool = Bool <$> arbitrary

genJsonNumber :: Gen Value
genJsonNumber = Number . realToFrac <$> (arbitrary :: Gen Double)

genJsonString :: Gen Value
genJsonString = String . take 20 <$> arbitrary

genJsonArray :: Gen Value -> Gen Value
genJsonArray g = Array <$> liftArbitrary g

genJsonObject :: Gen Value
genJsonObject =
  Object <$> (scale (`div` 4) . liftArbitrary $ oneof [genJsonBool, genJsonNumber, genJsonString])
