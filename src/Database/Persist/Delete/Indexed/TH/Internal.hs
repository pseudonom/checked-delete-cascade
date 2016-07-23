module Database.Persist.Delete.Indexed.TH.Internal where

import Control.Monad.Trans.Indexed.Log
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Database.Persist as P
import Language.Haskell.TH
import Language.Haskell.TH.ExpandSyns

import Database.Persist.Delete.Indexed

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> a = (f <$>) <$> a

type EntityType = Type
type PreReqType = Type
type Constructor = Con

pluck :: Dec -> Q (PreReqType, [(EntityType, Constructor)])
pluck dec = (entityType dec, ) . catMaybes <$> mapM fieldKeyConstructors (entityFieldConstructors dec)

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

fieldKeyConstructors :: Constructor -> Q (Maybe (EntityType, Constructor))
fieldKeyConstructors con =
  case con of
    (ForallC [] [AppT _equalityT ty] con') ->
      ((, con') <$$>) . traverse expandSyns . extractEntityType =<< expandSyns ty
    _ -> pure Nothing
  where
    extractEntityType (AppT (ConT k) ty)
      | k == ''P.Key = Just ty
    extractEntityType _ = Nothing
