{-# LANGUAGE BangPatterns, FlexibleContexts     #-}
{-# LANGUAGE TupleSections, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances                  #-}
module Document.Machine where

    --
    -- Modules
    --
import Document.Expression
import Document.Visitor
import Document.Proof
import Document.Refinement as Ref
import Document.Context

import Latex.Parser

import Logic.Expr
import Logic.ExpressionStore ( ExprStore )
import Logic.Proof hiding ( with_line_info )

import SummaryGen

import UnitB.AST
import UnitB.PO

import Theories.Arithmetic
import Theories.FunctionTheory
import Theories.IntervalTheory
import Theories.PredCalc
import Theories.SetTheory

import Z3.Z3 

    --
    -- Libraries
    --
import           Control.Monad hiding ( guard )
import           Control.Monad.Trans.State ( runStateT )
import qualified Control.Monad.Reader.Class as R
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader ( runReaderT )
import           Control.Monad.Trans.RWS as RWS
import           Control.Monad.Trans.State ( StateT )

import           Data.Char
import           Data.Functor.Identity
import           Data.Graph
import           Data.Map  as M hiding ( map, foldl, (\\) )
import           Data.Maybe as M ( maybeToList, isJust, isNothing ) 
import qualified Data.Maybe as M
import           Data.List as L hiding ( union, insert, inits )
import qualified Data.Set as S

--import Debug.Trace

import Utilities.Format
import Utilities.Syntactic
--import Utilities.Trace

list_file_obligations :: FilePath
                       -> IO (Either [Error] [(Machine, Map Label Sequent)])
list_file_obligations fn = do
        ct <- readFile fn
        return $ list_proof_obligations fn ct

list_proof_obligations :: FilePath -> String
                       -> Either [Error] [(Machine, Map Label Sequent)]
list_proof_obligations fn ct = do
        xs <- list_machines fn ct
        forM xs $ \x -> do
            y <- proof_obligation x
            return (x,y)

list_machines :: FilePath -> String 
              -> Either [Error] [Machine]
list_machines fn ct = do
        xs <- latex_structure fn ct
        ms <- all_machines xs
        return $ map snd $ toList $ machines ms
        
parse_rule' :: (Monad m)
            => String
            -> RuleParserParameter
            -> EitherT [Error] (RWST LineInfo [Error] System m) Rule
parse_rule' rule param = do
    li <- lift $ ask
    case M.lookup rule refinement_parser of
        Just f -> EitherT $ mapRWST (\x -> return (runIdentity x)) $
            runEitherT $ f rule param
        Nothing -> left [Error (format "invalid refinement rule: {0}" rule) li]

refinement_parser :: Map String (
                  String
               -> RuleParserParameter
               -> EitherT [Error] (RWS LineInfo [Error] System) Rule)
refinement_parser = fromList 
    [   ("disjunction", parse (disjunction, ()))
    ,   ("discharge", parse_discharge)
    ,   ("monotonicity", parse (Monotonicity, ()))
    ,   ("implication", parse (Implication, ()))
    ,   ("trading", parse (NegateDisjunct, ()))
    ,   ("transitivity", parse (Transitivity, ()))
    ,   ("psp", parse (PSP, ()))
    ,   ("ensure", parse (ensure, ()))
    ,   ("induction", parse_induction)
    ]

data HintBuilder = HintBuilder [LatexDoc] Machine

ensure :: ProgressProp 
       -> Label -> Event 
       -> HintBuilder
       -> EitherT [Error] (RWS LineInfo [Error] System) Ensure
ensure prog@(LeadsTo fv _ _) lbl evt (HintBuilder thint m) = do
        hint <- toEither $ tr_hint m (symbol_table fv) lbl thint empty_hint
        return $ Ensure prog lbl evt hint

instance RuleParser (a,()) => RuleParser (HintBuilder -> a,()) where
    parse_rule (f,_) xs rule param@(RuleParserParameter m _ _ _ _ hint) = do
        parse_rule (f (HintBuilder hint m),()) xs rule param

check_acyclic :: (Monad m) => String 
              -> [(Label,Label)] 
              -> LineInfo
              -> EitherT [Error] (RWST b [Error] d m) ()
check_acyclic x es li = do
        let cs = cycles es
        toEither $ mapM_ (cycl_err_msg x) cs
    where
        cycle_msg x ys = format msg (x :: String) $ intercalate ", " (map show ys)
        cycl_err_msg _ (AcyclicSCC _) = return ()
        cycl_err_msg x (CyclicSCC vs) = tell [Error (cycle_msg x vs) li]
        msg = "A cycle exists in the {0}: {1}"

trickle_down
        :: Monad m
        => Map Label Label 
        -> Map String a 
        -> (a -> a -> Either [String] a) 
        -> LineInfo
        -> EitherT [Error] m (Map String a)
trickle_down s ms f li = do
            let rs = map (\(AcyclicSCC v) -> v) $ cycles $ toList s
            foldM (\ms n -> 
                    case M.lookup n s of
                        Just anc  -> do
                            m <- hoistEither $ either 
                                (Left . map (\x -> Error x li)) Right 
                                $ f (ms ! show n) (ms ! show anc)
                            return $ insert (show n) m ms
                        Nothing -> return ms
                    ) ms rs

all_machines :: [LatexDoc] -> Either [Error] System
all_machines xs = do
        ms <- x
        return $ s { machines = ms }
    where
        (x,s,_) = runRWS (runEitherT $ read_document xs) () empty_system

produce_summaries :: System -> IO ()
produce_summaries sys = 
        void $ runStateT (do
            let ms = machines sys
            forM_ (M.elems ms) $ \m -> 
                forM_ (toList $ events m) $ \(lbl,evt) -> do
                    xs <- focus' (summary lbl evt :: (StateT ExprStore IO) String)
                    liftIO $ writeFile (show (_name m) ++ "_" ++ rename lbl ++ ".tex") xs
            ) sys
    where
        rename lbl = map f $ show lbl
        f ':' = '-'
        f x   = x
        
read_document :: [LatexDoc]
              -> EitherT [Error] (RWS () [Error] System) (Map String Machine)
read_document xs = do
--            traceM "step A"
            ms <- foldM gather empty xs 
            lift $ RWS.modify (\s -> s { 
                machines = ms })
            ms <- toEither $ foldM (f type_decl) ms xs
            toEither $ forM_ xs (g ctx_type_decl)
            refs  <- lift $ RWS.gets ref_struct
            let li = line_info xs
            check_acyclic "refinement structure" (toList refs) li
            ms <- trickle_down refs ms merge_struct li

                -- take actual generic parameter from `type_decl'
            ms <- toEither $ foldM (f imports) ms xs
            toEither $ mapM_ (g ctx_imports) xs
            ms <- trickle_down refs ms merge_import li
    
                -- take the types from `imports' and `type_decl`
            ms <- toEither $ foldM (f declarations) ms xs
--            toEither $ mapM_ (g ctx_operators) xs
            toEither $ mapM_ (g ctx_declarations) xs
            ms <- trickle_down refs ms merge_decl li
--            traceM "step M"
--            traceM $ show $ M.map (M.elems . variables) ms
                
                -- use the `declarations' of variables to check the
                -- type of expressions
            ms <- toEither $ foldM (f collect_expr) ms xs
            toEither $ mapM_ (g ctx_collect_expr) xs
            ms <- trickle_down refs ms merge_exprs li
--            traceM "step Q"
            
                -- use the label of expressions from `collect_expr' 
                -- and properties
            ms <- toEither $ foldM (f collect_refinement) ms xs
--            traceM "step QR"
            ms <- trickle_down refs ms merge_refinements li
            
                -- use the label of expressions from `collect_expr' 
                -- and the refinement rules
                -- in hints.
            toEither $ mapM_ (g ctx_collect_proofs) xs
            ms <- toEither $ foldM (f collect_proofs) ms xs
            cs <- lift $ gets theories
            toEither $ forM_ (toList cs) $ \(name,ctx) -> do
                let li = line_info xs
                fromEither () $ check_acyclic 
                    ("proofs of " ++ name)
                    (thm_depend ctx) li
--            toEither $ forM_ (toList ms) $ \(name,m) -> do
--                let li = line_info xs
--                fromEither () $ check_acyclic 
--                    ("proofs of " ++ name)
--                    (thm_depend ctx) li
--            traceM "step R"
            ms <- trickle_down refs ms merge_proofs li
--            traceM "step T"
            toEither $ forM_ (M.elems ms) 
                $ deduct_schedule_ref_struct li
            s  <- lift $ RWS.gets proof_struct
            check_acyclic "proof of liveness" s li
--            traceM "step Z"
            return ms
    where
        gather ms (Env n _ c li)     
                | n == "machine"    = do
                    (name,_) <- with_line_info li $ get_1_lbl c
                    let m           = empty_machine name
                    return (insert name m ms)
                | n == "context"    = do
                    (name,_) <- with_line_info li $ get_1_lbl c
                    let c           = empty_theory 
                                { extends = fromList [("basic",basic_theory)] }
                    lift $ modify $ \s -> s 
                        { theories = insert name c $ theories s }
                    return ms
                | otherwise         = foldM gather ms c
        gather ms x                 = fold_docM gather ms x
        f pass ms (Env n _ c li)     
                | n == "machine"    = do
                    fromEither ms (do
                        (name,cont) <- with_line_info li $ get_1_lbl c
                        m           <- toEither $ pass cont (ms ! name)
                        return (insert name m ms))
                | otherwise         = foldM (f pass) ms c
        f pass ms x                 = fold_docM (f pass) ms x
        g pass (Env n _ c li)     
                | n == "context"    = do
                    fromEither () (do
                        (name,cont) <- with_line_info li $ get_1_lbl c
                        c           <- lift $ gets $ (! name) . theories
                        c           <- toEither $ pass name cont c
                        lift $ modify $ \s -> s 
                            { theories = insert name c $ theories s } )
                | otherwise         = mapM_ (g pass) c
        g pass x                 = map_docM (g pass) x >> return ()

type_decl :: [LatexDoc] -> Machine -> MSEither Error System Machine
type_decl = visit_doc []
            [  (  "\\newset"
               ,  CmdBlock $ \(String name, String tag,()) m -> do
                    let th = theory m
                    let new_sort = Sort tag name 0
                    let new_type = Gen $ USER_DEFINED new_sort []
                    toEither $ error_list
                        [ ( tag `member` all_types th
                          , format "a sort with name '{0}' is already declared" tag )
                        , ( tag `member` consts th
                          , format "a constant with name '{0}' is already declared" tag )
                        ]
                    let dummy = Var "x@@" new_type
                    let new_const = Var name $ set_type new_type
                    let hd = th 
                            {  types = insert 
                                    tag
                                    new_sort
                                    (types th) 
                            ,  consts = insert tag new_const $ consts th
                            ,  fact = insert 
                                    (label (tag ++ "-def"))
                                    (fromJust $ mzforall [dummy] mztrue 
                                        (zelem 
                                            (Right $ Word dummy) 
                                            (Right $ Word new_const)))
                                    (fact th)
                            }
                    return m { theory = hd } 
               )
            ,  (  "\\newevent"
               ,  CmdBlock $ \(evt,()) m -> do 
                        toEither $ error_list
                            [ ( evt `member` events m
                              , format "event '{0}' is already defined" evt )
                            ]
                        return m { events = insert evt (empty_event) $ events m } 
               )
            ,  (  "\\refines"
               ,  CmdBlock $ \(mch,()) m -> do
                        anc   <- lift $ gets ref_struct
                        sys   <- lift $ gets machines
                        li    <- lift $ ask
                        unless (show mch `member` sys) $ left [Error (format "Machine {0} refines a non-existant machine: {1}" (_name m) mch) li]
                        when (_name m `member` anc) $ left [Error (format "Machines can only refine one other machine") li]
                        lift $ modify $ \x -> x { ref_struct = insert (_name m) mch $ ref_struct x }
                        return m
               )
            ]

imports :: Monad m 
        => [LatexDoc] 
        -> Machine 
        -> MSEitherT Error System m Machine 
imports = visit_doc []
            [   ( "\\with"
                , CmdBlock $ \(String th_name,()) m -> do
                    let th = [ ("sets"       , set_theory)
                             , ("functions"  , function_theory)
                             , ("arithmetic" , arithmetic)
                             , ("predcalc"   , pred_calc)
                             , ("intervals"  , interval_theory) ]
                        msg = "Undefined theory: {0} "
                            -- add suggestions
                    li <- lift $ ask
                    case th_name `L.lookup` th of
                        Nothing -> left [Error (format msg th_name) li]
                        Just th -> 
                            return m { theory = (theory m) {
                                extends = insert th_name th 
                                    $ extends (theory m) } }
                )
            ]

    -- Todo: detect when the same variable is declared twice
    -- in the same declaration block.
declarations :: Monad m
             => [LatexDoc] 
             -> Machine 
             -> MSEitherT Error System m Machine
declarations = visit_doc []
        [   (   "\\variable"
            ,   CmdBlock $ \(xs,()) m -> do
                        let msg = "repeated declaration: {0}"
                        vs <- get_variables 
                            (context m) 
                            (all_notation m) xs
                        let inter =                  S.fromList (map fst vs) 
                                    `S.intersection` keysSet (variables m)
                        toEither $ error_list 
                            [ ( not $ S.null inter
                              , format msg (intercalate ", " $ S.toList inter ))
                            ]
                        return m { variables = fromList vs `union` variables m} 
            )
        ,   (   "\\indices"
            ,   CmdBlock $ \(evt,xs,()) m -> do
                        let msg = "repeated declaration: {0}"
                        vs <- get_variables 
                            (context m) 
                            (all_notation m) xs
                        old_event <- bind 
                            (format "event '{0}' is undeclared" evt)
                            $ evt `M.lookup` events m
                        let var_names = map fst vs
                            inter = S.fromList var_names `S.intersection` 
                                    (           keysSet (params old_event) 
                                      `S.union` keysSet (indices old_event) )
                        toEither $ error_list
                            [ ( not $ S.null inter
                              , format msg $ intercalate ", " $ S.toList inter )
                            ]
                        let new_event = old_event { 
                            indices = fromList vs `union` indices old_event }
                        return m { events = insert evt new_event $ events m } 
            )
        ,   (   "\\param"
            ,   CmdBlock $ \(evt,xs,()) m -> do
                        
                        vs <- get_variables 
                            (context m) 
                            (all_notation m) xs
                        old_event <- bind
                            (format "event '{0}' is undeclared" evt)
                            $ evt `M.lookup` events m
                        let var_names = map fst vs
                            inter = S.fromList var_names `S.intersection` 
                                    (           keysSet (params old_event) 
                                      `S.union` keysSet (indices old_event) )
                            msg = "repeated declaration: {0}"
                        toEither $ error_list
                            [ ( not $ S.null inter
                              , format msg (intercalate ", " $ S.toList inter) )
                            ]
                        let new_event = old_event { 
                            params = fromList vs `union` params old_event }
                        return m { events = insert evt new_event $ events m } 
            )
        ,   (   "\\constant"
            ,   CmdBlock $ \(xs,()) m -> do
                        let err = "repeated definition"
                        vs <- get_variables 
                            (context m) 
                            (all_notation m) xs
                        return m { theory = (theory m) { 
                                consts = merge 
                                    (fromListWith (error err) vs) 
                                    (consts $ theory m) } } 
            )
        ,   (   "\\dummy"
            ,   CmdBlock $ \(xs,()) m -> do
                        let err = "repeated definition"
                        vs <- get_variables 
                            (context m) 
                            (all_notation m) xs
                        return m { theory = (theory m) { 
                                dummies = merge 
                                    (fromListWith (error err) vs) 
                                    (dummies $ theory m) } } 
            )
        ]

tr_hint :: Monad m
        => Machine
        -> Map String Var
        -> Label
        -> [LatexDoc]
        -> TrHint
        -> RWST LineInfo [Error] System m TrHint
tr_hint m vs lbl = visit_doc []
        [ ( "\\index"
          , CmdBlock $ \(String x, xs, ()) (TrHint ys z) -> do
                evt <- bind
                    (format "'{0}' is not an event of '{1}'" lbl $ _name m)
                    $ lbl `M.lookup` events m
                expr <- get_expr_with_ctx m 
                    (Context M.empty vs M.empty M.empty M.empty) xs
                toEither $ error_list 
                    [ ( not $ x `member` indices evt 
                      , format "'{0}' is not an index of '{1}'" x lbl )
                    ]
                return $ TrHint (insert x expr ys) z)
        , ( "\\lt"
          , CmdBlock $ \(prog,()) (TrHint ys z) -> do
                let msg = "Only one progress property needed for '{0}'"
                toEither $ error_list 
                    [ ( not $ isNothing z
                      , format msg lbl )
                    ]
                return $ TrHint ys (Just prog))
        ]
    

    -- Todo: report an error if we create two assignments (or other events 
    -- elements)
    -- with the same name
    -- Todo: guard the `insert` statements with checks for name clashes
    -- Todo: check scopes
collect_expr :: Monad m
             => [LatexDoc] 
             -> Machine 
             -> MSEitherT Error System m Machine
collect_expr = visit_doc 
                --------------
                --  Events  --
                --------------
        [] [(   "\\evassignment"
            ,   CmdBlock $ \(ev, lbl, xs,()) m -> do
                        let msg = "{0} is already used for another assignment"
                        old_event <- bind
                            (format "event '{0}' is undeclared" ev)
                            $ ev `M.lookup` events m
                        act     <- get_evt_part m old_event xs
                        new_act <- bind 
                            (format msg lbl)
                            $ insert_new lbl act (action old_event)
                        let new_event = old_event 
                                    { action = new_act }
                        scope (context m) act (        params old_event 
                                               `merge` indices old_event)
                        return m {          
                                events  = insert ev new_event $ events m } 
            )
        ,   (   "\\evguard"
            ,   CmdBlock $ \(evt, lbl, xs,()) m -> do
                        old_event <- bind
                            ( format "event '{0}' is undeclared" evt )
                            $ evt `M.lookup` events m
                        let grds = guards old_event
                            msg = "'{0}' is already used for another guard"
                        grd       <- get_evt_part m old_event xs
                        new_guard <- bind (format msg lbl)
                            $ insert_new lbl grd grds 
                        let n         = length $ sched_ref old_event
                            rule      = add_guard evt lbl
                            new_event = old_event
                                        { sched_ref = rule : sched_ref old_event
                                        , guards    = new_guard  }
                            po_lbl    = composite_label 
                                        [ evt, label "GRD"
                                        , _name m, label $ show n]
                        scope (context m) grd (        indices old_event 
                                               `merge` params old_event)
                        return m  
                              { props = (props m) { 
                                    derivation = 
                                        insert po_lbl (Rule rule)
                                    $ derivation (props m) } 
                              , events  = insert evt new_event $ events m } 
            )
        ,   (   "\\cschedule"
            ,   CmdBlock $ \(evt, lbl, xs,()) m -> do
                        old_event <- bind 
                            ( format "event '{0}' is undeclared" evt )
                            $ evt `M.lookup` events m
                        let msg = "'{0}'' is already used for another schedule"
                        sch <- get_evt_part m old_event xs
                        new_sched <- bind
                            (format msg lbl)
                            $ insert_new lbl sch $ scheds old_event
                        let new_event = old_event { 
                                    scheds = new_sched }
                        scope (context m) sch (indices old_event) 
                        return m {          
                                events  = insert evt new_event $ events m } 
            )
        ,   (   "\\fschedule"
            ,   CmdBlock $ \(evt, lbl :: Label, xs,()) m -> do
                        old_event <- bind
                            (format "event '{0}' is undeclared" evt)
                            $ evt `M.lookup` events m
                        let msg = "{0} is already used for another schedule"
                        sch       <- get_evt_part m old_event xs
                        new_sched <- bind 
                            (format msg lbl)
                            $ insert_new lbl sch $ scheds old_event
                        let new_event = old_event { 
                                    scheds = new_sched }
                        scope (context m) sch (indices old_event) 
                        return m {          
                                events  = insert evt new_event $ events m } 
            )
                -------------------------
                --  Theory Properties  --
                -------------------------
        ,   (   "\\assumption"
            ,   CmdBlock $ \(lbl,xs,()) m -> do
                        let th = theory m
                            msg = "'{0}' is already used for another assertion"
                        axm      <- get_assert m xs
                        new_fact <- bind (format msg lbl)
                            $ insert_new lbl axm $ fact th
                        return m { 
                            theory = th { fact = new_fact } } 
            )
                --------------------------
                --  Program properties  --
                --------------------------
        ,   (   "\\initialization"
            ,   CmdBlock $ \(lbl,xs,()) m -> do
                        let msg = "'{0}' is already used for another invariant"
                        initp     <- get_assert m xs
                        new_inits <- bind (format msg lbl)
                            $ insert_new lbl initp $ inits m
                        return m {
                                inits = new_inits } 
            )
        ,   (   "\\invariant"
            ,   CmdBlock $ \(lbl,xs,()) m -> do
                        let msg = "'{0}' is already used for another invariant"
                        invar   <- get_assert m xs
                        new_inv <- bind (format msg lbl)
                            $ insert_new lbl invar $ inv $ props m
                        return m { 
                            props = (props m) { 
                                inv = new_inv } } 
            )
        ,   (   "\\transient"      
            ,   CmdBlock $ \(ev, lbl, xs,()) m -> do
                        let msg = "'{0}' is already used for another\
                                  \ program property"
                        toEither $ error_list
                            [ ( not (ev `member` events m)
                              , format "event '{0}' is undeclared" ev )
                            ]
                        tr            <- get_assert_with_free m xs
                        let prop = Transient 
                                    (free_vars (context m) tr) 
                                    tr ev empty_hint
                            old_prog_prop = transient $ props m
                        new_props     <- bind (format msg lbl)
                            $ insert_new lbl prop $ old_prog_prop
                        return m {
                            props = (props m) {
                                transient = new_props } } 
            )
        ,   (   "\\transientB"      
            ,   CmdBlock $ \(ev, lbl, hint, xs,()) m -> do
                        let msg = "'{0}' is already used for\
                                  \ another program property"
                        toEither $ error_list
                            [ ( not (ev `member` events m)
                              , format "event '{0}' is undeclared" ev )
                            ]
                        tr            <- get_assert_with_free m xs
                        let fv = (free_vars (context m) tr)
                        hint <- toEither $ tr_hint 
                                            m fv ev hint empty_hint
                        let prop = Transient fv tr ev hint
                            old_prog_prop = transient $ props m
                        new_props  <- bind (format msg lbl)
                                $ insert_new lbl prop $ old_prog_prop
                        return m {
                            props = (props m) {
                                transient = new_props } } 
            )
        ,   (   "\\constraint"
            ,   CmdBlock $ \(lbl,xs,()) m -> do
                        let msg = "'{0}' is already used for another safety property"
                        pre <- get_predicate m empty_ctx WithFreeDummies xs
                        let vars = elems $ free_vars (context m) pre
                        new_cons <- bind (format msg lbl)
                                $ insert_new lbl (Co vars pre) 
                                    $ constraint $ props m
                        return m { 
                            props = (props m) { 
                                constraint = new_cons } } 
            )
        ,   (   "\\safety"
            ,   CmdBlock $ \(lbl, pCt, qCt,()) m -> do
                    (p,q)    <- toEither (do
                        p    <- fromEither ztrue $ get_assert_with_free m pCt
                        q    <- fromEither ztrue $ get_assert_with_free m qCt
                        return (p,q))
                    let ctx = context m
                        dum = free_vars ctx p `union` free_vars ctx q
                        new_prop = Unless (M.elems dum) p q Nothing
                    new_saf_props <- bind (format "safety property '{0}' already exists" lbl)
                        $ insert_new lbl 
                                new_prop
                                (safety $ props m)
                    return m { props = (props m) 
                        { safety = new_saf_props } } 
            )
        ,   (   "\\progress"
            ,   CmdBlock $ \(lbl, pCt, qCt,()) m -> do
                    let prop = progress $ props m
                    (p,q)    <- toEither (do
                        p    <- fromEither ztrue $ get_assert_with_free m pCt
                        q    <- fromEither ztrue $ get_assert_with_free m qCt
                        return (p,q))
                    let ctx = context m
                        dum = S.fromList (elems $ free_vars ctx p) 
                                `S.union` S.fromList (elems $ free_vars ctx q)
                        new_prop = LeadsTo (S.elems dum) p q
                    new_prog <- bind (format "progress property '{0}' already exists" lbl)
                        $ insert_new lbl new_prop $ prop 
                    new_deriv <- bind (format "proof step '{0}' already exists" lbl)
                        $ insert_new lbl (Rule Add) $ derivation $ props m
                    return m { props = (props m) 
                        { progress   = new_prog
                        , derivation = new_deriv
                        } } 
            )
        ]

scope :: (Monad m, R.MonadReader LineInfo m)
      => Context -> Expr -> Map String Var 
      -> EitherT [Error] m ()
scope ctx xp vs = do
    let fv          = keysSet $ free_vars ctx xp
    let decl_v      = keysSet vs
    let undecl_v    = S.toList (fv `S.difference` decl_v)
    li             <- R.ask
    if fv `S.isSubsetOf` decl_v
    then return ()
    else left [Error (format "Undeclared variables: {0}" 
                      (intercalate ", " undecl_v)) li]

collect_refinement :: Monad m 
                   => [LatexDoc] 
                   -> Machine
                   -> MSEitherT Error System m Machine 
collect_refinement = visit_doc []
        [   (   "\\refine"
            ,   CmdBlock $ \(goal, String rule, hyps, hint, ()) m -> do
                    toEither $ error_list
                        [   ( not (goal `member` (progress (props m) `union` progress (inh_props m)))
                            , format "the goal is an undefined progress property {0}" goal )
                        ]
                    let prog = progress (props m) `union` progress (inh_props m)
                        saf  = safety (props m) `union` safety (inh_props m)
                    r <- parse_rule' (map toLower rule) 
                        (RuleParserParameter m prog saf goal hyps hint)
                    return m { props = (props m) { derivation = insert goal r $ derivation $ props m } } 
            )
        ,   (   "\\safetyB"
            ,   CmdBlock $ \(lbl, evt, pCt, qCt, ()) m -> do
                        -- Why is this here instead of the expression collector?
                    _  <- bind
                        (format "event '{0}' is undeclared" evt)
                        $ evt `M.lookup` events m
                    let prop = safety $ props m
                    (p,q) <- toEither (do
                        p <- fromEither ztrue $ get_assert_with_free m pCt
                        q <- fromEither ztrue $ get_assert_with_free m qCt
                        error_list 
                            [   ( lbl `member` prop
                                , format "safety property '{0}' already exists" lbl )
                            ] 
                        return (p,q))
                    let ctx = context m
                        dum =       free_vars ctx p 
                            `union` free_vars ctx q
                    let new_prop = Unless (M.elems dum) p q (Just evt)
                    return m { props = (props m) 
                        { safety = insert lbl new_prop $ prop 
                        } } 
            )
        ,   (   "\\replace"
            ,   CmdBlock $ \(evt,del,add,keep,prog,saf,()) m -> do
                    old_event <- bind
                        (format "event '{0}' is undeclared" evt)
                        $ evt `M.lookup` events m
                    let sc    = scheds old_event
                        lbls  = (S.elems $ add `S.union` del `S.union` keep)
                        progs = progress (props m) `union` progress (inh_props m)
                        safs  = safety (props m) `union` safety (inh_props m)
                    _     <- bind_all lbls
                        (format "'{0}' is not a valid schedule")
                        $ (`M.lookup` sc)
                    pprop <- bind 
                        (format "'{0}' is not a valid progress property" prog)
                        $ prog `M.lookup` progs
                    sprop <- bind
                        (format "'{0}' is not a valid safety property" saf)
                        $ saf `M.lookup` safs
                    let n         = length $ sched_ref old_event
                        rule      = (replace evt (prog,pprop) (saf,sprop)) 
                                    { remove = del
                                    , add = add
                                    , keep = keep }
                        new_event = old_event { sched_ref = rule : sched_ref old_event }
                        po_lbl    = composite_label [evt,label "SCH",_name m,label $ show n]
                    return m 
                      { events = insert evt new_event $ events m
                      , props = (props m) { 
                            derivation = 
                                insert po_lbl (Rule rule)
                            $ derivation (props m) } 
                      }
            )
        ,   (   "\\weakento"
            ,   CmdBlock $ \(evt :: Label,del :: S.Set Label,add :: S.Set Label,()) m -> do
                    old_event <- bind
                        (format "event '{0}' is undeclared" evt)
                        $ evt `M.lookup` events m
                    let sc        = scheds old_event
                        lbls      = (S.elems $ add `S.union` del)
                    _     <- bind_all lbls
                        (format "'{0}' is not a valid schedule")
                        $ (`M.lookup` sc)
                    let n         = length $ sched_ref old_event
                        rule      = (weaken evt)
                                    { remove = del
                                    , add = add }
                        new_event = old_event 
                                    { sched_ref = rule : sched_ref old_event }
                        po_lbl    = composite_label [evt,label "SCH",_name m,label $ show n]
                    return m 
                      { events = insert evt new_event $ events m
                      , props = (props m) { 
                            derivation = 
                                insert po_lbl (Rule rule)
                            $ derivation (props m) } 
                      }
            )
        ,   (   "\\replacefine"
            ,   CmdBlock $ \(evt, keep, old, new, prog, ()) m -> do
                    old_event <- bind
                        (format "event '{0}' is undeclared" evt)
                        $ evt `M.lookup` events m
                    let sc        = scheds old_event
                        progs     = progress (props m) `union` progress (inh_props m)
                    _     <- bind_all (S.elems keep)
                        (format "'{0}' is not a valid schedule")
                        $ (`M.lookup` sc)
                    pprop <- bind 
                        (format "'{0}' is not a valid progress property" prog)
                        $ prog `M.lookup` progs
                    old_exp <- bind
                        (format "'{0}' is not a valid schedule" $ M.fromJust old)
                        $ maybe (Just ztrue) (`M.lookup` sc) old
                    new_exp <- bind 
                        (format "'{0}' is not a valid schedule" $ M.fromJust new)
                        $ maybe (Just ztrue) (`M.lookup` sc) new
                    let n         = length $ sched_ref old_event
                        rule      = (replace_fine evt old_exp new new_exp (prog,pprop))
--                                    { add = S.fromList $ maybeToList new
--                                    , remove = S.fromList $ maybeToList old 
--                                    , keep = keep }
                        new_event = old_event 
                                    { sched_ref = rule : sched_ref old_event }
                        po_lbl    = composite_label [evt,label "SCH",_name m,label $ show n]
                    return m 
                      { events = insert evt new_event $ events m
                      , props = (props m) { 
                            derivation = 
                                insert po_lbl (Rule rule)
                            $ derivation (props m) } 
                      }
            )
        ,   (   "\\removeguard"
            ,   CmdBlock $ \(evt, lbls, ()) m -> do
                    old_event <- bind
                        (format "event '{0}' is undeclared" evt)
                        $ evt `M.lookup` events m
                    let grd       = guards old_event
                    toEither $ do
                        error_list $ flip map lbls $ \lbl ->
                            ( not $ lbl `member` grd
                                , format "'{0}' is not a valid schedule" lbl )
                    let n         = length $ sched_ref old_event
                        rules     = map (remove_guard evt) lbls
                        new_event = old_event 
                                    { sched_ref = rules ++ sched_ref old_event }
                        po_lbl    = flip map [n .. ] $ \n -> 
                                    composite_label [evt,label "GRD",_name m,label $ show n]
                    return m 
                      { events = insert evt new_event $ events m
                      , props = (props m) { 
                            derivation = 
                                     M.fromList (zip po_lbl $ map Rule rules)
                            `union`  derivation (props m) } 
                      }
            )
        ]

collect_proofs :: Monad m 
               => [LatexDoc] 
               -> Machine
               -> MSEitherT Error System m Machine
collect_proofs = visit_doc
        [   (   "proof"
            ,   EnvBlock $ \(po,()) xs m -> do -- with_tracingM $ do
                        -- This should be moved to its own phase
                    let po_lbl = label $ remove_ref $ concatMap flatten po
                        lbl = composite_label [ _name m, po_lbl ]
                        th  = theory m
                    toEither $ error_list 
                        [   ( lbl `member` proofs (props m)
                            , format "a proof for {0} already exists" lbl )
                        ] 
                    li <- lift $ ask
                    s  <- bind 
                        (format "proof obligation does not exist: {0} {1}" 
                                lbl 
                                (M.keys $ raw_machine_pos m))
                        $ lbl `M.lookup` raw_machine_pos m
                    p <- runReaderT (
                            runEitherT $
                            run_visitor li xs $ 
                            collect_proof_step (empty_pr th) 
                            ) th
                    p <- EitherT $ return p
                    p <- EitherT $ return $ runTactic li s p
                    return m { 
                        props = (props m) { 
                            proofs = insert lbl p $ 
                                    proofs $ props m } } 
            )
        ] []

deduct_schedule_ref_struct :: Monad m
                           => LineInfo -> Machine
                           -> RWST r [Error] System m ()
deduct_schedule_ref_struct li m = do
        forM_ (toList $ events m) check_sched
        forM_ (toList $ transient $ props m) check_trans
    where
        check_trans (lbl,Transient _ _ evt (TrHint _ lt))  = do
                add_proof_edge lbl [g evt $ _name m]
                let f_sch = fine $ new_sched (events m ! evt)
                    progs = progress (props m) `union` progress (inh_props m) 
                unless (maybe True (flip member progs) lt)
                    $ tell [Error (format "'{0}' is not a progress property" $ M.fromJust lt) li]
                unless (isJust f_sch == isJust lt)
                    $ if isJust f_sch
                    then tell [Error (format fmt0 lbl evt) li]
                    else tell [Error (format fmt1 lbl evt) li]
                add_proof_edge lbl $ maybeToList lt
            where
                fmt0 =    "transient predicate {0}: a leads-to property is required for "
                       ++ "transient predicates relying on events "
                       ++ "({1}) with a fine schedule"
                fmt1 =    "transient predicate {0}: a leads-to property is only required "
                       ++ "for events ({1}) with a fine schedule"
        check_sched (lbl,evt) = do
                ref <- gets ref_struct
                case M.lookup (_name m) ref of
                    Just m' -> do
                        add_proof_edge (g lbl m') [g lbl $ _name m]
                        mapM_ (f (g lbl m')) $ sched_ref evt
                    Nothing ->
                        return ()
        f lbl cs = 
            case rule cs of
                Weaken -> return ()
                Replace (prog,_) (saf,_) -> 
                    add_proof_edge lbl [prog,saf]
                ReplaceFineSch _ _ _ (prog,_) ->
                    add_proof_edge lbl [prog]
                RemoveGuard _ -> return ()
                AddGuard _ -> return ()
        g lbl m = composite_label [m, lbl, label "SCH"]

parse_system :: FilePath -> IO (Either [Error] System)
parse_system fn = runEitherT $ do
        xs <- EitherT $ parse_latex_document fn
        hoistEither $ all_machines xs
        
parse_machine :: FilePath -> IO (Either [Error] [Machine])
parse_machine fn = runEitherT $ do
        xs <- EitherT $ parse_latex_document fn
        ms <- hoistEither $ all_machines xs
        return $ map snd $ toList $ machines ms








        