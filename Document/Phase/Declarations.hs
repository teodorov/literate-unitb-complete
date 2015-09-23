{-# LANGUAGE TypeOperators, RecursiveDo, LambdaCase, TupleSections, FlexibleContexts #-}
module Document.Phase.Declarations where

    --
    -- Modules
    --
import Document.Expression
import Document.Pipeline
import Document.Phase as P
import Document.Proof
import Document.Scope
import Document.VarScope
import Document.Visitor

import Latex.Parser hiding (contents)

import Logic.Expr

import UnitB.AST as AST

    --
    -- Libraries
    --
import Control.Arrow hiding (left,app) -- (Arrow,arr,(>>>))
import qualified Control.Category as C
import           Control.Applicative 

import           Control.Monad 
import           Control.Monad.Reader.Class 
import           Control.Monad.Reader (Reader,runReader)
import           Control.Monad.Writer.Class 
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe

import Control.Lens as L hiding ((|>),(<.>),(<|),indices,Context)

import           Data.Default
import           Data.Either
import           Data.Map   as M hiding ( map, foldl, (\\) )
import qualified Data.Map   as M
import qualified Data.Maybe as MM
import           Data.List as L hiding ( union, insert, inits )
import qualified Data.Traversable as T

import qualified Utilities.BipartiteGraph as G
import Utilities.Format
import Utilities.Syntactic
  
run_phase2_vars :: Pipeline MM 
                        (Hierarchy MachineId,MTable MachineP1)
                        (MTable MachineP2)
run_phase2_vars = second (C.id &&& symbols) >>> liftP wrapup
    where
        err_msg = format "Multiple symbols with the name {0}"
        wrap = L.map (second $ VarScope . uncurry3 TheoryDef)
        symbols = run_phase
            [ variable_decl
            , constant_decl
            , dummy_decl
            , index_decl
            , arr $ Just . M.map (wrap . L.view pSetDecl)
            , param_decl
            , remove_var ]
        wrapup (r_ord,(p1,vs)) = do
            let names = M.map (view pEventRenaming) p1
                vs' = inherit2 names r_ord 
                        <$> unionsWith (++)
                        <$> vs
            vars <- triggerM
                =<< make_all_tables' err_msg 
                =<< triggerM vs'
                    
            let _  = vars :: MTable (Map String VarScope)
            T.sequence $ make_phase2 <$> p1 <.> vars

make_phase2 :: MachineP1
            -> Map String VarScope
            -> MM MachineP2 
make_phase2 p1 vars = mdo
        -- tell err
        -- unless (L.null err) $ MaybeT $ return Nothing
        let 
            newThy t = makeTheoryP2 t _pNotation _pCtxSynt <$> liftField toThyDecl (M.toList vars)
            _pNotation  = th_notation $ empty_theory { extends = p2 ^. pImports }
            _pCtxSynt   = mkSetting _pNotation (p1 ^. pTypes) constants M.empty (p2' ^. pDummyVars)
        p2  <- p1 & pContext newThy
        p2' <- p2 & pEventRef (mapEvents (liftEvent toOldEventDecl) (liftEvent toNewEventDecl))
        let 
            _ = p2' :: MachineP1' EventP2 TheoryP2
            _pMchSynt   = mkSetting _pNotation (p1 ^. pTypes) constants refVars (p2 ^. pDummyVars)
            refVars = (p2''^.pAbstractVars) `M.union` (p2''^.pStateVars)
            constants = (p2^.pConstants) `M.union` (M.mapMaybe defToVar $ p2 ^. pDefinitions)
        p2'' <- makeMachineP2' p2' _pMchSynt 
                <$> liftField toMchDecl (M.toList vars)
                     -- & pEventRef %~ G.mapBothWithKey (liftEvent toOldEventDecl) 
                     --                                 (liftEvent toNewEventDecl)
        let 
            liftEvent :: (String -> VarScope -> [Either Error (EventId, [EventP2Field])])
                      -> Maybe EventId -> EventP1 -> MM EventP2
            liftEvent f = \case 
                    Just eid -> \e -> makeEventP2 e (_pEvtSynt eid) (_pSchSynt eid) . (!eid) <$> m  -- (m ! eid)
                    Nothing -> \e -> return $ makeEventP2 e def def []
                where
                    m = M.fromListWith (++) <$> liftField f (M.toList vars)
            ind_param :: EventId -> Map String Var
            ind_param eid = M.union (p2''^.getEvent eid.eIndices) (p2''^.getEvent eid.eParams)
            parser :: (EventId -> Map String Var)
                   -> EventId -> ParserSetting
            parser table e    = mkSetting _pNotation (p2' ^. pTypes) (constants `union` table e) refVars (p2' ^. pDummyVars)
            _pSchSynt :: EventId -> ParserSetting
            _pSchSynt = parser (\eid -> p2''^.getEvent eid.eIndices)

            _pEvtSynt :: EventId -> ParserSetting
            _pEvtSynt = parser ind_param
            
        -- | L.null err = 
        return p2''  -- & (pNotation .~ _pNotation)
                     -- & (pMchSynt .~ _pMchSynt)
                     -- & (pCtxSynt .~ _pCtxSynt)
                     -- & (pSchSynt .~ _pSchSynt)
                     -- & (pEvtSynt .~ _pEvtSynt)
    where
        -- varGroup n (VarScope x) = VarGroup [(n,x)]
        -- vars' = groupVars $ L.map (uncurry varGroup) $ M.toList vars
        -- err = []
        -- (p2',err) = execRWS (mapM_ f vars') () p2
        --     where
        --         p2 =   pContext `over` makeTheoryP2
        --              $ pEvents `over` M.map makeEventP2
        --              $ makeMachineP2' p1

        -- f (VarGroup vs) = processDecl vs

        -- evts  = M.map (const ()) (p1 ^. pEvents)

        -- findE m e = findWithDefault M.empty e m :: Map String Var
        
        -- event_namespace :: Map EventId (Map String Var) -> EventId -> ParserSetting
        -- event_namespace table = 
        --     _ $ M.mapWithKey (const . parser table) evts 

instance IsVarScope TheoryDef where
    toOldEventDecl _ _ = []
    toNewEventDecl _ _ = []
    toThyDecl s th = [Right $ PDefinitions s $ thDef th]
    toMchDecl _ _  = []
    -- processDecl xs = do
    --     let xs' = M.fromList $ L.map (second thDef) xs
    --     pDefinitions %= M.union xs'

variable_decl :: MPipeline MachineP1
                    [(String,VarScope)]
variable_decl = machine_var_decl Machine "\\variable"

instance IsVarScope TheoryConst where
    toOldEventDecl _ _ = []
    toNewEventDecl _ _ = []
    toThyDecl s th = [Right $ PConstants s $ thCons th]
    toMchDecl _ _  = []
    -- processDecl xs = do
    --     let xs' = M.fromList $ L.map (second thCons) xs
    --     pConstants %= M.union xs'

constant_decl :: MPipeline MachineP1
                    [(String,VarScope)]
constant_decl = machine_var_decl TheoryConst "\\constant"

instance IsVarScope MachineVar where
    toOldEventDecl _ _ = []
    toNewEventDecl _ _ = []
    toThyDecl _ _ = []
    toMchDecl s (Machine v Local _)     = [Right $ PStateVars s v]
    toMchDecl s (Machine v Inherited _) = map Right [PAbstractVars s v,PStateVars s v]
    toMchDecl s (DelMch (Just v) Local li)     = map Right [PDelVars s (v,li),PAbstractVars s v]
    toMchDecl s (DelMch (Just v) Inherited li) = [Right $ PDelVars s (v,li)]
    toMchDecl s (DelMch Nothing _ li)    = [Left $ Error (format "deleted variable '{0}' does not exist" s) li]
    -- toMchDecl _ _ = Nothing
    -- processDecl xs = do
    --     let f :: (String,MachineVar) 
    --           -> Either Error ( [(String,(Var,LineInfo))]
    --                           , [(String,Var)]
    --                           , [(String,Var)])
    --         f (n,Machine v Local _) = Right ([],[],[(n,v)])
    --         f (n,Machine v Inherited _) = Right ([],[(n,v)],[(n,v)])
    --         f (n,DelMch (Just v) Local li) = Right ([(n,(v,li))],[(n,v)],[])
    --         f (n,DelMch (Just v) Inherited li) = Right ([(n,(v,li))],[],[])
    --         f (n,DelMch Nothing _ li) = Left $ Error (format "deleted variable '{0}' does not exist" n) li
    --         ys = map f xs
    --         (del,abst,st) = (_1 `over` M.fromList)
    --                         $ (both `over` M.fromList) 
    --                         $ mconcat $ rights ys
    --         zs = lefts ys
    --     tell zs
    --     pAbstractVars %= M.union abst
    --     pDelVars   %= M.union del
    --     pStateVars %= M.union st


remove_var :: MPipeline MachineP1 [(String,VarScope)]
remove_var = machineCmd "\\removevar" $ \(One xs) _m _p1 -> do
        li <- lift ask
        return $ map ((\x -> (x,VarScope $ DelMch Nothing Local li)) . toString) xs

dummy_decl :: MPipeline MachineP1
                    [(String,VarScope)]
dummy_decl = machine_var_decl 
        (\v source li -> Evt $ singleton Nothing (v,Param,source,li)) 
        "\\dummy"

machine_var_decl :: IsVarScope var
                 => (Var -> DeclSource -> LineInfo -> var)
                 -> String
                 -> MPipeline MachineP1
                        [(String,VarScope)]
machine_var_decl scope kw = machineCmd kw $ \(One xs) _m p1 -> do
            vs <- get_variables' (p1 ^. pAllTypes) xs
            li <- lift ask
            return $ map (\(x,y) -> (x,VarScope $ scope y Local li)) vs

index_decl :: MPipeline MachineP1 [(String,VarScope)]
index_decl = event_var_decl Index "\\indices"

param_decl :: MPipeline MachineP1 [(String,VarScope)]
param_decl = event_var_decl Param "\\param"

type EventSym = (EventId,[(String,Var)])

toEventDecl :: String -> EvtDecl -> [Either a (EventId,[EventP2Field])]
toEventDecl s (Evt m) = map Right $ L.map (second $ (:[]).f)
                                     $ MM.mapMaybe (\(x,y) -> (,y) <$> x) $ M.toList m
         where 
            f :: (Var, EvtScope, DeclSource, LineInfo) -> EventP2Field
            f x = case x^._2 of 
                        Index -> EIndices s $ x^._1
                        Param -> EParams s $ x^._1

instance IsVarScope EvtDecl where
    toOldEventDecl = toEventDecl
    toNewEventDecl = toEventDecl
    toMchDecl _ _  = []
    toThyDecl n (Evt m) = L.map (Right . PDummyVars n . view _1) $ M.elems 
                                $ M.filterWithKey (const.MM.isNothing) m
    -- processDecl xs = do
    --     let f (n,Evt m) = mconcat $ M.elems $ M.mapWithKey (g n) m
    --         g :: String -> Maybe EventId 
    --           -> (Var,EvtScope,DeclSource,LineInfo)
    --           -> ([EventSym],[EventSym],[(String,Var)])
    --         g n (Just eid) (v,Index,_,_) = ([(eid,[(n,v)])],[],[])  
    --         g n (Just eid) (v,Param,_,_) = ([],[(eid,[(n,v)])],[])  
    --         g n Nothing (v,_,_,_) = ([],[],[(n,v)])
    --         (is,ps,ds) = 
    --                   _1 `over` (M.map M.fromList . M.fromListWith (++)) 
    --                 $ _2 `over` (M.map M.fromList . M.fromListWith (++)) 
    --                 $ _3 `over` M.fromList
    --                 $ mconcat $ map f xs
    --     pIndices %= M.unionWith M.union is
    --     pParams  %= M.unionWith M.union ps
    --     pDummyVars %= M.union ds

event_var_decl :: EvtScope
               -> String
               -> MPipeline MachineP1
                    [(String,VarScope)]
event_var_decl escope kw = machineCmd kw $ \(lbl,xs) _m p1 -> do
            let ts   = L.view pTypes p1
                evts = L.view pEventIds p1 
            evt <- bind
                (format "event '{0}' is undeclared" lbl)
                $ lbl `M.lookup` evts
            li <- lift ask
            vs <- get_variables' ts xs
            return $ map (\(n,v) -> ((n,VarScope $ Evt $ M.singleton (Just evt) (v,escope,Local,li)))) vs

defToVar :: Def -> Maybe Var
defToVar (Def _ n [] t _) = Just (Var n t)
defToVar (Def _ _ (_:_) _ _) = Nothing
