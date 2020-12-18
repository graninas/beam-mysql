{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

module Main (main) where

import           Control.Exception.Safe (bracket)
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity)
import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                Table (PrimaryKey, primaryKey), TableEntity,
                                defaultDbSettings)
import qualified Database.Beam as B
import           Database.Beam.MySQL (MySQL, runBeamMySQL)
import           Database.Beam.MySQL (runInsertRowReturning)
import           Database.Beam.Query (SqlSelect, select)
import           Database.MySQL.Base (MySQLConn, Query (Query), close, connect,
                                      execute_)
import           Database.MySQL.Temp (toConnectInfo, withTempDB)
import           GHC.Generics (Generic)
import           Test.Hspec (Spec, describe, hspec, it, shouldReturn)

type Correct = CorrectT Identity

main :: IO ()
main = do
  hspec $ spec mkSpecWithConn
  where
    mkSpecWithConn :: (MySQLConn -> IO Correct) -> IO (CorrectT Identity)
    mkSpecWithConn runQuery = withTempDB $ \db
      -> bracket (connect . toConnectInfo $ db)
                    close
                    (\conn -> setUpDB conn >>
                              runQuery conn)
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
      "bad_text varchar(255) not null, " <>
      "bad_text2 text not null" <>
      ");"
    insertCorrect :: Query
    insertCorrect = Query $
      "insert into correct_table (" <>
      "id, bad_text, bad_text2)"  <>
      "values (" <>
      "DEFAULT," <>
      "\'foo\\'\'," <>
      bobbyTablesBS <>
      ")"

runSelectQuery :: MySQLConn -> IO (CorrectT Identity)
runSelectQuery conn = do
  res <- runBeamMySQL conn . B.runSelectReturningList $ query
  case res of
    []     -> fail "Expected query result, but got nothing."
    res':_ -> pure $ res'
  where
    query :: SqlSelect MySQL (CorrectT Identity)
    query = select . B.limit_ 1 . B.filter_' predicate $ B.all_ (_testCorrectTable testDB)
      where predicate CorrectT {_correctBadText2} =
              _correctBadText2 B.==?. B.val_ bobbyTables

runInsertQuery :: MySQLConn -> IO Correct
runInsertQuery conn = do
  runBeamMySQL conn . B.runInsert $ query
  pure val
  where
    val = CorrectT 10 "hello" bobbyTables
    query = B.insert (_testCorrectTable testDB)
          $ B.insertExpressions [ (B.val_ val) ]

runUpdateQuery :: MySQLConn -> IO Correct
runUpdateQuery conn = do
  runBeamMySQL conn . B.runUpdate
                    $ B.update'
                    (_testCorrectTable testDB)
                    (\tbl -> _correctBadText tbl B.<-. (B.val_ bobbyTables))
                    (\tbl ->  _correctBadText2 tbl B.==?. (B.val_ bobbyTables))
  runSelectQuery conn

runSaveQuery :: MySQLConn -> IO Correct
runSaveQuery conn = do
  runBeamMySQL conn . B.runUpdate
                    $ B.save' (_testCorrectTable testDB) (CorrectT 1 "hello" bobbyTables)
  runSelectQuery conn

runInsertReturningQuery :: MySQLConn -> IO Correct
runInsertReturningQuery conn =
  runBeamMySQL conn (runInsertRowReturning query) >>= \case
    Nothing -> fail "Couldn't insert value"
    Just res -> pure res
  where
    query = B.insert (_testCorrectTable testDB)
          $ B.insertExpressions [B.val_ (CorrectT 2 "hello" bobbyTables)]

bobbyTables :: Text
bobbyTables = "\'; DROP TABLE students; --"

bobbyTablesBS :: BSL.ByteString
bobbyTablesBS = "\'\\'; DROP TABLE students; --\'"

spec :: ((MySQLConn -> IO Correct) -> IO Correct) -> Spec
spec mkSpec = do
  describe "Text escaping should prevent little Bobby Tables from dropping the students table" $ do
    it "via select . filter_" $ do
      _correctBadText2 <$> (mkSpec runSelectQuery) `shouldReturn` bobbyTables
    it "via insert" $ do
      _correctBadText2 <$> (mkSpec runInsertQuery) `shouldReturn` bobbyTables
    it "via update " $ do
      _correctBadText2 <$> (mkSpec runUpdateQuery) `shouldReturn` bobbyTables
    it "via save" $ do
      _correctBadText2 <$> (mkSpec runSaveQuery) `shouldReturn` bobbyTables
    it "via insertReturning" $ do
      _correctBadText2 <$> (mkSpec runInsertReturningQuery) `shouldReturn` bobbyTables

data CorrectT (f :: Type -> Type) = CorrectT
  {
    _correctId       :: Columnar f Int64,
    _correctBadText  :: Columnar f Text,
    _correctBadText2 :: Columnar f Text
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
