{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}

module Main (main) where

import           Control.Exception.Safe (bracket)
import           Control.Monad (void)
import           Criterion.Main (bench, defaultMain, nfIO)
import           Data.Foldable (traverse_)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                QExpr, SqlInsert, SqlInsertValues,
                                Table (PrimaryKey, primaryKey), TableEntity,
                                defaultDbSettings, default_, insert,
                                insertExpressions, runInsert, val_)
import           Database.Beam.MySQL (MySQL, runBeamMySQL,
                                      runInsertRowReturning)
import           Database.MySQL.Base (MySQLConn, Query (Query), close, connect,
                                      execute_)
import           Database.MySQL.Temp (toConnectInfo, withTempDB)
import           Fmt (Builder, padLeftF, (+|), (|+))
import           GHC.Generics (Generic)

main :: IO ()
main = withTempDB (\db -> do
  bracket (connect . toConnectInfo $ db)
          close
          go)
  where
    go :: MySQLConn -> IO ()
    go conn = do
      setUpDB conn
      putStrLn "Benchmarking inserts."
      defaultMain [
        bench "Raw" . nfIO . rawInsert $ conn,
        bench "Beam, no return" . nfIO . beamInsert $ conn,
        bench "Beam, returning" . nfIO . beamReturningInsert $ conn
        ]

-- Helpers

setUpDB :: MySQLConn -> IO ()
setUpDB conn = traverse_ (execute_ conn) [
  "create database test;",
  "use test",
  dbExpr]
  where
    dbExpr :: Query
    dbExpr = Query $
      "create table test_table (id bigint(20) primary key auto_increment" +|
      commaSep toColName [1 .. 37] |+
      ");"

rawInsert :: MySQLConn -> IO ()
rawInsert conn = void . execute_ conn . Query $
  "insert into test_table (id" +|
  commaSep toColNameNoType [1 .. 37] |+
  ") values (default " +|
  commaSep quote [1 .. 37] |+
  ");"

beamReturningInsert :: MySQLConn -> IO ()
beamReturningInsert conn =
  void . runBeamMySQL conn . runInsertRowReturning $ theInsert

beamInsert :: MySQLConn -> IO ()
beamInsert conn = runBeamMySQL conn . runInsert $ theInsert

theInsert :: SqlInsert MySQL TestT
theInsert = insert (_testTestTable testDb) exprs
  where
    exprs :: SqlInsertValues MySQL (TestT (QExpr MySQL s))
    exprs = insertExpressions [TestT default_
                 (val_ "col01")
                 (val_ "col02")
                 (val_ "col03")
                 (val_ "col04")
                 (val_ "col05")
                 (val_ "col06")
                 (val_ "col07")
                 (val_ "col08")
                 (val_ "col09")
                 (val_ "col10")
                 (val_ "col11")
                 (val_ "col12")
                 (val_ "col13")
                 (val_ "col14")
                 (val_ "col15")
                 (val_ "col16")
                 (val_ "col17")
                 (val_ "col18")
                 (val_ "col19")
                 (val_ "col20")
                 (val_ "col21")
                 (val_ "col22")
                 (val_ "col23")
                 (val_ "col24")
                 (val_ "col25")
                 (val_ "col26")
                 (val_ "col27")
                 (val_ "col28")
                 (val_ "col29")
                 (val_ "col30")
                 (val_ "col31")
                 (val_ "col32")
                 (val_ "col33")
                 (val_ "col34")
                 (val_ "col35")
                 (val_ "col36")
                 (val_ "col37")]

toColNameNoType :: Word -> Builder
toColNameNoType i = "col" +| padLeftF 2 '0' i |+ ""

toColName :: Word -> Builder
toColName i = "col" +| padLeftF 2 '0' i |+ " varchar(255) not null"

quote :: Word -> Builder
quote i = "'" +| padLeftF 2 '0' i |+ "'"

commaSep :: (Foldable t) => (a -> Builder) -> t a -> Builder
commaSep f = foldMap (\x -> ", " +| f x)

-- Table
data TestT f =
  TestT
  { _testId    :: Columnar f Int,
    _testCol01 :: Columnar f Text,
    _testCol02 :: Columnar f Text,
    _testCol03 :: Columnar f Text,
    _testCol04 :: Columnar f Text,
    _testCol05 :: Columnar f Text,
    _testCol06 :: Columnar f Text,
    _testCol07 :: Columnar f Text,
    _testCol08 :: Columnar f Text,
    _testCol09 :: Columnar f Text,
    _testCol10 :: Columnar f Text,
    _testCol11 :: Columnar f Text,
    _testCol12 :: Columnar f Text,
    _testCol13 :: Columnar f Text,
    _testCol14 :: Columnar f Text,
    _testCol15 :: Columnar f Text,
    _testCol16 :: Columnar f Text,
    _testCol17 :: Columnar f Text,
    _testCol18 :: Columnar f Text,
    _testCol19 :: Columnar f Text,
    _testCol20 :: Columnar f Text,
    _testCol21 :: Columnar f Text,
    _testCol22 :: Columnar f Text,
    _testCol23 :: Columnar f Text,
    _testCol24 :: Columnar f Text,
    _testCol25 :: Columnar f Text,
    _testCol26 :: Columnar f Text,
    _testCol27 :: Columnar f Text,
    _testCol28 :: Columnar f Text,
    _testCol29 :: Columnar f Text,
    _testCol30 :: Columnar f Text,
    _testCol31 :: Columnar f Text,
    _testCol32 :: Columnar f Text,
    _testCol33 :: Columnar f Text,
    _testCol34 :: Columnar f Text,
    _testCol35 :: Columnar f Text,
    _testCol36 :: Columnar f Text,
    _testCol37 :: Columnar f Text
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)

instance Table TestT where
  data PrimaryKey TestT f = TestTPK (Columnar f Int)
                            deriving stock (Generic)
                            deriving anyclass (Beamable)
  primaryKey = TestTPK . _testId

-- Database
newtype TestDB f =
  TestDB
    { _testTestTable :: f (TableEntity TestT)
    }
  deriving stock (Generic)
  deriving anyclass (Database MySQL)

testDb :: DatabaseSettings MySQL TestDB
testDb = defaultDbSettings
