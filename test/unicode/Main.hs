{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Main (main) where

import           Control.Exception.Safe (bracket)
import           Control.Monad (join)
import           Data.Char (GeneralCategory (NotAssigned, PrivateUse),
                            generalCategory)
import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity)
import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (Text, chunksOf, filter, pack)
import           Data.Vector (Vector, fromList, imap, toList)
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                Table (PrimaryKey, primaryKey), TableEntity,
                                defaultDbSettings)
import           Database.Beam.MySQL (MySQL, ViaJson (ViaJson), runBeamMySQL)
import           Database.Beam.Query (QExpr, SqlInsertValues, all_, asc_,
                                      insert, insertValues, orderBy_, runInsert,
                                      runSelectReturningList, select)
import           Database.MySQL.Base (MySQLConn, Query (Query), close, connect,
                                      execute_)
import           Database.MySQL.Temp (MySQLDB, toConnectInfo, withTempDB)
import           GHC.Generics (Generic)
import           Prelude hiding (filter)
import           Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = do
  res <- withTempDB go
  hspec . spec $ res
  where
    go :: MySQLDB -> IO (Vector (Text, Text))
    go db = bracket (connect . toConnectInfo $ db)
                    close
                    (\conn -> setUpDB conn >> insertSample conn >> retrieveSample conn)

-- Helpers

spec :: Vector (Text, Text) -> Spec
spec v = describe "Unicode round-tripping" $ do
  it "should round-trip properly" $ do
    v `shouldBe` sampleData

-- every valid Unicode code point, in chunks of 32
sampleData :: Vector (Text, Text)
sampleData =
  fmap (join (,)) .
  fromList .
  chunksOf 32 .
  filter go .
  pack $ [minBound .. maxBound]
  where
    go :: Char -> Bool
    go c = case generalCategory c of
      NotAssigned -> False
      PrivateUse  -> False
      _           -> True

setUpDB :: MySQLConn -> IO ()
setUpDB conn = traverse_ (execute_ conn) [
  "create database test;",
  "use test",
  makeTestTable]
  where
    makeTestTable :: Query
    makeTestTable = Query $
      "create table test_table (" <>
      "id bigint primary key not null, " <>
      "data varchar(255) not null, " <>
      "json varchar(255) not null);"

insertSample :: MySQLConn -> IO ()
insertSample conn = runBeamMySQL conn . runInsert . insert (_testTestTable testDB) $ go
  where
    go :: SqlInsertValues MySQL (TestT (QExpr MySQL s))
    go = insertValues . toList . imap buildTestT $ sampleData
    buildTestT :: Int -> (Text, Text) -> TestT Identity
    buildTestT i (t, t') = TestT (fromIntegral i) t (ViaJson t')

retrieveSample :: MySQLConn -> IO (Vector (Text, Text))
retrieveSample conn =
  fmap (fmap go . fromList) . runBeamMySQL conn . runSelectReturningList . select $ do
    res <- orderBy_ (asc_ . _testId) . all_ . _testTestTable $ testDB
    pure (_testData res, _testJson res)
  where
    go :: (Text, ViaJson Text) -> (Text, Text)
    go (t, ViaJson t') = (t, t')

-- Beam boilerplate

data TestT (f :: Type -> Type) = TestT
  {
    _testId   :: Columnar f Int64,
    _testData :: Columnar f Text,
    _testJson :: Columnar f (ViaJson Text)
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
