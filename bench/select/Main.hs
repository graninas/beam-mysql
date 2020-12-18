{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Main (main) where

import           Control.Exception.Safe (bracket)
import           Control.Monad (void)
import           Criterion.Main (bench, defaultMain, nfIO)
import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                Table (PrimaryKey, primaryKey), TableEntity,
                                defaultDbSettings)
import           Database.Beam.MySQL (MySQL, runBeamMySQL)
import           Database.Beam.Query (SqlSelect, all_, guard_,
                                      runSelectReturningList, select, val_,
                                      (<.))
import           Database.MySQL.Base (MySQLConn, Query (Query), close, connect,
                                      execute_, queryVector_, skipToEof)
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
      putStrLn "Benchmarking selects."
      defaultMain [
        bench "Raw, all" . nfIO . rawAllSelect $ conn,
        bench "Raw, qualified" . nfIO . rawQualifiedSelect $ conn,
        bench "Beam, all" . nfIO . beamAllSelect $ conn,
        bench "Beam, qualified" . nfIO . beamQualifiedSelect $ conn
        ]

-- Helpers

setUpDB :: MySQLConn -> IO ()
setUpDB conn = traverse_ (execute_ conn) [
  "create database test;",
  "use test",
  createDB,
  populateDB]
  where
    createDB :: Query
    createDB = Query $
      "create table test_table (id bigint(20) primary key auto_increment" +|
      commaSep toColName [1 .. 37] |+
      ");"
    populateDB :: Query
    populateDB = Query $
      "insert into test_table (id" +|
      commaSep toCol [1 .. 37] |+
      ") values " +|
      valuesStmt 1 |+
      ";"

rawAllSelect :: MySQLConn -> IO ()
rawAllSelect conn =
  bracket (queryVector_ conn query) (skipToEof . snd) (skipToEof . snd)
  where
    query :: Query
    query = Query "select * from test_table;"

rawQualifiedSelect :: MySQLConn -> IO ()
rawQualifiedSelect conn =
  bracket (queryVector_ conn query) (skipToEof . snd) (skipToEof . snd)
  where
    query :: Query
    query = Query "select * from test_table where id < 500;"

beamAllSelect :: MySQLConn -> IO ()
beamAllSelect conn = void . runBeamMySQL conn . runSelectReturningList $ sel
  where
    sel :: SqlSelect MySQL (TestT Identity)
    sel = select . all_ . _testTestTable $ testDB

beamQualifiedSelect :: MySQLConn -> IO ()
beamQualifiedSelect conn = void . runBeamMySQL conn . runSelectReturningList $ sel
  where
    sel :: SqlSelect MySQL (TestT Identity)
    sel = select $ do
      res <- all_ . _testTestTable $ testDB
      guard_ (_testId res <. val_ 500)
      pure res

valuesStmt :: Word -> Builder
valuesStmt i =
  "(default" +|
  commaSep go [1 .. 37] |+
  ") "
  where
    go :: Word -> Builder
    go _ = "'" +| i |+ "'"

toCol :: Word -> Builder
toCol i = "col" +| padLeftF 2 '0' i |+ ""

toColName :: Word -> Builder
toColName i = "col" +| padLeftF 2 '0' i |+ " varchar(255) not null"

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

testDB :: DatabaseSettings MySQL TestDB
testDB = defaultDbSettings
