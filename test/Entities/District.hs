{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Entities.District where

import Data.Text
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "districtMigrate", mkDeleteCascade sqlSettings] [persistUpperCase|
  District
    name Text
    deriving Show Eq
|]
