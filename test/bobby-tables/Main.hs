{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}

module Main (main) where

import           Control.Exception.Safe (bracket)
import           Data.Foldable (traverse_)
import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                Table (PrimaryKey, primaryKey), TableEntity,
                                defaultDbSettings)
import           Database.Beam.MySQL (MySQL, runBeamMySQL,
                                      runInsertRowReturning)
import           Database.Beam.Query (QAssignment, QBaseScope, QExpr, QField,
                                      QGenExpr, QValueContext, SqlInsert,
                                      SqlSelect, SqlUpdate, all_, guard_,
                                      insert, insertValues, limit_, runInsert,
                                      runSelectReturningOne, runUpdate, save,
                                      select, update, val_, (<-.), (==.))
import           Database.MySQL.Base (MySQLConn, Query (Query), close, connect,
                                      execute_)
import           Database.MySQL.Temp (toConnectInfo, withTempDB)
import           GHC.Generics (Generic)
import           Test.Hspec (Spec, describe, hspec, it, shouldReturn)

main :: IO ()
main = hspec . spec $ go
  where
    go :: (MySQLConn -> IO Text) -> IO Text
    go callback =
      withTempDB $ \db -> bracket (connect . toConnectInfo $ db)
                                  close
                                  (\conn -> setUpDB conn >> callback conn)

-- Helpers

spec :: ((MySQLConn -> IO Text) -> IO Text) -> Spec
spec callback = do
  describe "Text escaping should prevent little Bobby Tables from dropping the students table" $ do
    it "via select . filter_" $ do
      callback runSelectQuery `shouldReturn` bobbyTables
    it "via insert" $ do
      callback runInsertQuery `shouldReturn` bobbyTables
    it "via update " $ do
      callback runUpdateQuery `shouldReturn` bobbyTables
    it "via save" $ do
      callback runSaveQuery `shouldReturn` bobbyTables
    it "via insertReturning" $ do
      callback runInsertReturningQuery `shouldReturn` bobbyTables

runSelectQuery :: MySQLConn -> IO Text
runSelectQuery conn = do
  res <- runBeamMySQL conn . runSelectReturningOne $ query
  case res of
    Nothing   -> fail "Expected query result, but got nothing."
    Just res' -> pure res'
  where
    query :: SqlSelect MySQL Text
    query = select $ do
      res <- limit_ 1 . all_ . _testCorrectTable $ testDB
      guard_ . predicate $ res
      pure . _correctBadText2 $ res
    predicate ::
      CorrectT (QGenExpr QValueContext MySQL QBaseScope) ->
      QGenExpr QValueContext MySQL QBaseScope Bool
    predicate row = _correctBadText2 row ==. val_ bobbyTables

runInsertQuery :: MySQLConn -> IO Text
runInsertQuery conn = do
  runBeamMySQL conn . runInsert $ query
  runSelectQuery conn
  where
    query :: SqlInsert MySQL CorrectT
    query =
      insert (_testCorrectTable testDB) .
        insertValues $ [CorrectT 10 "hello" bobbyTables]

runUpdateQuery :: MySQLConn -> IO Text
runUpdateQuery conn = do
  runBeamMySQL conn . runUpdate $ go
  runSelectQuery conn
  where
    go :: SqlUpdate MySQL CorrectT
    go = update (_testCorrectTable testDB) tableMod tablePred
    tableMod :: forall s . CorrectT (QField s) -> QAssignment MySQL s
    tableMod row = _correctBadText row <-. val_ bobbyTables
    tablePred :: forall s . CorrectT (QExpr MySQL s) -> QExpr MySQL s Bool
    tablePred row = _correctBadText2 row ==. val_ bobbyTables

runSaveQuery :: MySQLConn -> IO Text
runSaveQuery conn = do
  runBeamMySQL conn . runUpdate $ go
  runSelectQuery conn
  where
    go :: SqlUpdate MySQL CorrectT
    go = save (_testCorrectTable testDB) (CorrectT 1 "hello" bobbyTables)

runInsertReturningQuery :: MySQLConn -> IO Text
runInsertReturningQuery conn = do
  res <- runBeamMySQL conn . runInsertRowReturning $ go
  case res of
    Nothing   -> fail "Expected query result, but got nothing."
    Just res' -> pure . _correctBadText2 $ res'
  where
    go :: SqlInsert MySQL CorrectT
    go =
      insert (_testCorrectTable testDB) .
      insertValues $ [CorrectT 2 "hello" bobbyTables]

bobbyTables :: Text
bobbyTables = "\'; DROP TABLE students; --"

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
      "\'\\'; DROP TABLE students; --\'" <>
      ");"

-- Beam boilerplate

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
