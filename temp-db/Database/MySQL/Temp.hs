{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.MySQL.Temp
(
  MySQLDB,
  toConnectInfo,
  withTempDB
) where

import           Control.Concurrent (threadDelay)
import           Control.Exception.Safe (MonadMask, bracket)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Kind (Type)
import           Data.String (fromString)
import           Data.Word (Word16)
import           Database.MySQL.Base (ConnectInfo (ciDatabase, ciHost, ciPort, ciUser),
                                      defaultConnectInfoMB4)
import           Fmt ((+|), (|+))
import           System.Directory (createDirectory, getCurrentDirectory,
                                   getTemporaryDirectory, removePathForcibly,
                                   setCurrentDirectory)
import           System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import           System.FilePath ((</>))
import           System.Posix.User (getLoginName)
import           System.Process.Typed (ProcessConfig, shell, startProcess,
                                       stopProcess, waitExitCode)
import           System.Random (getStdRandom, random)

data MySQLDB = MySQLDB {
  port   :: {-# UNPACK #-} !Word16,
  user   :: !String,
  socket :: !FilePath
  }
  deriving stock (Eq, Show)

toConnectInfo :: MySQLDB -> ConnectInfo
toConnectInfo db = defaultConnectInfoMB4 {
  ciHost = "localhost",
  ciPort = fromIntegral . port $ db,
  ciDatabase = "",
  ciUser = fromString . user $ db
  }

withTempDB :: forall (a :: Type) (m :: Type -> Type) .
  (MonadIO m, MonadMask m) => (MySQLDB -> m a) -> m a
withTempDB act = withTempDBDir go
  where
    go :: FilePath -> m a
    go tmpDir = do
      userName <- liftIO getLoginName
      let initDBCmd = buildInitCommand userName tmpDir
      currDir <- liftIO getCurrentDirectory
      bracket (liftIO . setCurrentDirectory $ tmpDir)
              (\_ -> liftIO . setCurrentDirectory $ currDir)
              (\_ -> do
                  proc <- startProcess initDBCmd -- will stop on its own
                  ec <- waitExitCode proc
                  case ec of
                    ExitSuccess             -> runTempDB act tmpDir userName
                    failure@(ExitFailure _) -> liftIO . exitWith $ failure)

-- Helpers

withTempDBDir :: (MonadIO m, MonadMask m) => (FilePath -> m a) -> m a
withTempDBDir act = do
  rand :: Word <- liftIO . getStdRandom $ random
  tmp <- liftIO getTemporaryDirectory
  let tempDir = tmp </> ("mysql" <> show rand)
  bracket (liftIO . createDirectory $ tempDir)
          (\_ -> liftIO . removePathForcibly $ tempDir)
          (\_ -> act tempDir)

runTempDB :: (MonadIO m, MonadMask m) =>
  (MySQLDB -> m a) -> FilePath -> String -> m a
runTempDB act tmpDir userName = do
  let sock = tmpDir </> "mysqld.sock"
  let runDBCmd = buildRunCommand userName tmpDir sock
  bracket (startProcess runDBCmd)
          stopProcess
          (\_ -> do
            liftIO . threadDelay $ 1000000 -- ensures readiness
            act . MySQLDB 3306 "root" $ sock)

buildInitCommand :: String -> FilePath -> ProcessConfig () () ()
buildInitCommand userName tmpDir = shell $
  "mysqld --initialize-insecure --user=" +|
  userName |+
  " --datadir=" +|
  tmpDir |+
  " --explicit-defaults-for-timestamp --log-error-verbosity=1"

buildRunCommand :: String -> FilePath -> String -> ProcessConfig () () ()
buildRunCommand userName tmpDir sock = shell $
  "mysqld --user=" +|
  userName |+
  " --datadir=" +|
  tmpDir |+
  " --socket=" +|
  sock |+
  " --log-error-verbosity=1"
