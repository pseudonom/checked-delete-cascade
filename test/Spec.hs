{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

import Test.Hspec

import Control.Monad (void)
import qualified Control.Monad.Indexed.Syntax as I
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT(..))
import Control.Monad.Trans.Indexed.Log
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT, MonadBaseControl)
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite (withSqlitePool)
import Database.Persist.TH
import Prelude
import Data.String (fromString)
import System.Log.FastLogger (fromLogStr)

db :: SqlPersistT (LoggingT (ResourceT IO)) a -> IO ()
db actions = runResourceT $ runConn $ actions >> transactionUndo

runConn :: (MonadIO m, MonadBaseControl IO m) => SqlPersistT (LoggingT m) t -> m ()
runConn f =
  void . flip runLoggingT (\_ _ _ s -> B8.putStrLn $ fromLogStr s) $
  withSqlitePool "test/testdb.sqlite3" 1 $
  runSqlPool f

share [mkPersist sqlSettings, mkMigrate "testMigrate"] [persistUpperCase|
  District
    name Text
    deriving Show Eq
  School
    name Text
    districtId DistrictId Maybe
    deriving Show Eq
  Teacher
    name Text
    schoolId SchoolId
    deriving Show Eq
|]

-- Manually splice in TH's `deleteCascade` instances
-- For clarity
instance (PersistQuery backend,
          PersistEntityBackend District ~ backend) =>
         DeleteCascade District backend where
  deleteCascade key_aeIH
    = do { deleteCascadeWhere
             [Filter SchoolDistrictId (Left (Just key_aeIH)) Eq];
           delete key_aeIH }
instance (PersistQuery backend,
          PersistEntityBackend School ~ backend) =>
         DeleteCascade School backend where
  deleteCascade key_aeII
    = do { deleteCascadeWhere
             [Filter TeacherSchoolId (Left key_aeII) Eq];
           delete key_aeII }
instance (PersistQuery backend,
          PersistEntityBackend Teacher ~ backend) =>
         DeleteCascade Teacher backend where
  deleteCascade key_aeIJ = do { delete key_aeIJ }

data Delete a = Delete

class DeleteCascadeI record backend (pointedAtBy :: [*]) | record -> pointedAtBy where
  deleteCascadeI
    :: (MonadIO m, (i `Covers` pointedAtBy) ~ 'True)
    => [Filter record] -> Performs (Delete record) (ReaderT backend m) i ()
instance DeleteCascadeI District SqlBackend '[Delete School] where
  deleteCascadeI f = logLift (Delete :: Delete District) $ deleteWhere f
instance DeleteCascadeI School SqlBackend '[Delete Teacher] where
  deleteCascadeI f = logLift (Delete :: Delete School) $ deleteWhere f
instance DeleteCascadeI Teacher SqlBackend '[] where
  deleteCascadeI f = logLift (Delete :: Delete Teacher) $ deleteWhere f

setup :: (MonadIO m) => SqlPersistT m DistrictId
setup = do
  dId <- insert $ District "districtName"
  sId <- insert $ School "schoolName" (Just dId)
  _ <- insert $ Teacher "teacherName" sId
  pure dId

data Unknown = Unknown

selectKeysListI
  :: (PersistEntityBackend a ~ backend, MonadIO m, ConsCollapse Unknown i, PersistQuery backend, PersistEntity a)
  => [Filter a] -> [SelectOpt a] -> IndexedLogT (ReaderT backend m) i (Unknown ::: i) [Key a]
selectKeysListI f s = logLift Unknown $ selectKeysList f s

main :: IO ()
main = do
  runConn $ runMigrationUnsafe testMigrate
  hspec $
    describe "persistent" $ do
      it "should silently delete other entities with the default delete cascade" $ db $ do
        dId <- setup
        [_] <- selectList ([] :: [Filter Teacher]) []
        deleteCascade dId
        [] <- selectList ([] :: [Filter Teacher]) []
        pure ()
      it "should fail to type check if we don't cascade" $ db $ do
        dId <- setup
        [_] <- selectList ([] :: [Filter Teacher]) []
        runIndexedLogT $ do
          let ((>>), (>>=)) = ((I.>>), (I.>>=))
          sIds <- selectKeysListI [SchoolDistrictId ==. Just dId] []
          deleteCascadeI [TeacherSchoolId <-. sIds]
          deleteCascadeI [SchoolDistrictId ==. Just dId]
          deleteCascadeI [DistrictId ==. dId]
        [] <- selectList ([] :: [Filter Teacher]) []
        pure ()
