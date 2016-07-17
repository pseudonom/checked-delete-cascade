{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Entities.Teacher where

import Data.Text
import Database.Persist.TH

import Entities.School

share [mkPersist sqlSettings, mkMigrate "teacherMigrate", mkDeleteCascade sqlSettings] [persistUpperCase|
  Teacher
    name Text
    schoolId SchoolId
    deriving Show Eq
|]
