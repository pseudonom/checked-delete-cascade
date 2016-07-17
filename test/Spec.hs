{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -F -pgmF modulate #-}

import Control.Monad (void)
import qualified Control.Monad.Indexed.Syntax as I
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT(..))
import Control.Monad.Trans.Indexed.Log
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT, MonadBaseControl)
import qualified Data.ByteString.Char8 as B8
import Data.String (fromString)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite (withSqlitePool)
import Prelude
import System.Log.FastLogger (fromLogStr)
import Test.Hspec

import Database.Persist.Delete.Indexed
import Database.Persist.Delete.Indexed.TH
import Entities.*

db :: SqlPersistT (LoggingT (ResourceT IO)) a -> IO ()
db actions = runResourceT $ runConn $ actions >> transactionUndo

runConn :: (MonadIO m, MonadBaseControl IO m) => SqlPersistT (LoggingT m) t -> m ()
runConn f =
  void . flip runLoggingT (\_ _ _ s -> B8.putStrLn $ fromLogStr s) $
  withSqlitePool "test/testdb.sqlite3" 1 $
  runSqlPool f

setup :: (MonadIO m) => SqlPersistT m DistrictId
setup = do
  dId <- insert $ District "districtName"
  sId <- insert $ School "schoolName" dId
  _ <- insert $ Teacher "teacherName" sId
  pure dId

data Select a = Select

selectKeysListI
  :: forall a backend m
  . (PersistEntityBackend a ~ backend, MonadIO m, PersistQuery backend, PersistEntity a)
  => [Filter a] -> [SelectOpt a] -> Performs (Select a) (ReaderT backend m) [Key a]
selectKeysListI f s = logLift (Select :: Select a) $ selectKeysList f s

$(mkInstances ''SqlBackend)

main :: IO ()
main = do
  runConn . runMigrationUnsafe $ do
    districtMigrate
    schoolMigrate
    teacherMigrate
  hspec $
    describe "persistent" $
      it "should fail to type check if we don't cascade" $ db $ do
        dId <- setup
        [_] <- selectList ([] :: [Filter Teacher]) []
        runIndexedLogT $ do
          let ((>>), (>>=)) = ((I.>>), (I.>>=))
          sIds <- selectKeysListI [SchoolDistrictId ==. dId] []
          deleteCascadeI [TeacherSchoolId <-. sIds]
          deleteCascadeI [SchoolDistrictId ==. dId]
          deleteCascadeI [DistrictId ==. dId]
        [] <- selectList ([] :: [Filter Teacher]) []
        pure ()
