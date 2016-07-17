{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Entities.School where

import Data.Text
import Database.Persist.TH

import Entities.District

share [mkPersist sqlSettings, mkMigrate "schoolMigrate", mkDeleteCascade sqlSettings] [persistUpperCase|
  School
    name Text
    districtId DistrictId
    deriving Show Eq
|]
