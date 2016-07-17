module Database.Persist.Delete.Indexed.TH.Internal where

import Control.Monad.Trans.Indexed.Log
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Database.Persist as P
import Language.Haskell.TH.Syntax

import Database.Persist.Delete.Indexed

type EntityType = Type
type PreReqType = Type
type Constructor = Con

pluck :: [Dec] -> [(PreReqType, [(EntityType, Constructor)])]
pluck = map (\i -> (entityType i, catMaybes $ fieldKeyConstructors <$> entityFieldConstructors i))

filterFKs :: [(PreReqType, [(EntityType, Constructor)])] -> [(PreReqType, [(EntityType, Constructor)])]
filterFKs =
  map (\(ConT ent, fields) -> (ConT ent, rejectId ent fields))
  where
    rejectId ent = reject (\(_, NormalC conName _) -> nameBase ent <> "Id" == nameBase conName)

reject :: (a -> Bool) -> [a] -> [a]
reject p = filter (not . p)

invert :: [(PreReqType, [(EntityType, Constructor)])] -> Map EntityType [PreReqType]
invert es =
  List.foldl' (\acc (pre, ent) -> Map.insertWith (<>) (simplifyName ent) [pre] acc) entities $
  concatMap (\(ent, fields) -> (ent, ) . fst <$> fields) es
  where
    entities = Map.fromList $ (\(ent, _) -> (simplifyName ent, [])) <$> es
    -- Necessary so that types imported from different modules still register as equal
    simplifyName (ConT nm) = ConT . mkName $ nameBase nm
    simplifyName _ = error "Not expecting anything other than `ConT`"

mkInstance :: Name -> EntityType -> [PreReqType] -> Q [Dec]
mkInstance backendName ent pointAtEnt =
  [d|
    instance DeleteCascadeI $(pure ent) $(pure $ promoteList prereqs) $(pure $ ConT backendName) where
      deleteCascadeI f = logLift (Delete :: Delete $(pure ent)) $ deleteWhere f
  |]
  where
    prereqs = AppT (ConT ''Delete) <$> pointAtEnt

promoteList :: [EntityType] -> Type
promoteList = List.foldl' (\list ty -> PromotedConsT `AppT` ty `AppT` list) PromotedNilT

entityFieldInstances :: Q [Dec]
entityFieldInstances = do
  FamilyI _ instances <- reify $ mkName "EntityField"
  pure instances

entityType :: Dec -> Type
entityType (DataInstD _ _ [ty, _] _ _) = ty
entityType _ = error "`EntityField` not returning `DataInstD`"

entityFieldConstructors :: Dec -> [Constructor]
entityFieldConstructors (DataInstD _ _ _ cons _) = cons
entityFieldConstructors _ = error "`EntityField` not returning `DataInstD`"

fieldKeyConstructors :: Constructor -> Maybe (Type, Constructor)
fieldKeyConstructors (ForallC [] [AppT _ (AppT (ConT k) ty)] con)
  | k == ''P.Key = Just (ty, con)
  | otherwise = Nothing
fieldKeyConstructors (ForallC [] [AppT _ (ConT ty)] con) =
  (, con) . ConT . mkName <$> "Id" `stripSuffix` show ty
fieldKeyConstructors _ = Nothing

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix xs ys = reverse <$> List.stripPrefix (reverse xs) (reverse ys)
