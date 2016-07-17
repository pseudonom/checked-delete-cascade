module Database.Persist.Delete.Indexed.TH where

import qualified Data.Map as Map
import Language.Haskell.TH.Syntax

import Database.Persist.Delete.Indexed.TH.Internal

mkInstances :: Name -> Q [Dec]
mkInstances backendName =
  fmap concat . mapM (uncurry (mkInstance backendName)) . Map.toList . invert . filterFKs . pluck =<< entityFieldInstances
