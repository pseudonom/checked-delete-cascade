{-# LANGUAGE TemplateHaskell #-}
module Lib where

-- import Database.Persist
-- import Database.Persist.Quasi
-- import Database.Persist.TH
-- import Language.Haskell.TH.Lib (varE)
-- import Language.Haskell.TH.Quote
-- import Language.Haskell.TH.Syntax

-- -- | Generate a 'DeleteCascade' instance for the given @EntityDef@s.
-- mkDeleteCascadeI :: MkPersistSettings -> [EntityDef] -> Q [Dec]
-- mkDeleteCascadeI mps defs = do
--     let deps = concatMap getDeps defs
--     mapM (go deps) defs
--   where
--     getDeps :: EntityDef -> [Dep]
--     getDeps def =
--         concatMap getDeps' $ entityFields $ fixEntityDef def
--       where
--         getDeps' :: FieldDef -> [Dep]
--         getDeps' field@FieldDef {..} =
--             case foreignReference field of
--                 Just name ->
--                      return Dep
--                         { depTarget = name
--                         , depSourceTable = entityHaskell def
--                         , depSourceField = fieldHaskell
--                         , depSourceNull  = nullable fieldAttrs
--                         }
--                 Nothing -> []
--     go :: [Dep] -> EntityDef -> Q Dec
--     go allDeps EntityDef{entityHaskell = name} = do
--         let deps = filter (\x -> depTarget x == name) allDeps
--         key <- newName "key"
--         let del = VarE 'delete
--         let dcw = VarE 'deleteCascadeWhere
--         just <- [|Just|]
--         filt <- [|Filter|]
--         eq <- [|Eq|]
--         left <- [|Left|]
--         let mkStmt :: Dep -> Stmt
--             mkStmt dep = NoBindS
--                 $ dcw `AppE`
--                   ListE
--                     [ filt `AppE` ConE filtName
--                            `AppE` (left `AppE` val (depSourceNull dep))
--                            `AppE` eq
--                     ]
--               where
--                 filtName = filterConName' mps (depSourceTable dep) (depSourceField dep)
--                 val (Nullable ByMaybeAttr) = just `AppE` VarE key
--                 val _                      =             VarE key



--         let stmts :: [Stmt]
--             stmts = map mkStmt deps `mappend`
--                     [NoBindS $ del `AppE` VarE key]

--         let entityT = genericDataType mps name backendT

--         return $
--             InstanceD
--             [ mkClassP ''PersistQuery [backendT]
--             , mkEqualP (ConT ''PersistEntityBackend `AppT` entityT) (ConT ''BaseBackend `AppT` backendT)
--             ]
--             (ConT ''DeleteCascade `AppT` entityT `AppT` backendT)
--             [ FunD 'deleteCascade
--                 [normalClause [VarP key] (DoE stmts)]
--             ]
