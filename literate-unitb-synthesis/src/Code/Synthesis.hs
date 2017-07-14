{-# LANGUAGE OverloadedStrings #-}
module Code.Synthesis where

    -- Modules
import Logic.Expr
import Logic.Proof
import qualified Logic.Proof.POGenerator as PG

import           Language.UnitB.PO 
import           Language.UnitB.Syntax as UB hiding (Event)

    -- Libraries
import Control.Arrow (first, (***))
import Control.Lens hiding (indices)

import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class

import Control.Monad.Trans
import Control.Monad.Trans.RWS    
        (RWS,RWST
        ,runRWS,runRWST
        ,execRWST,execRWS)

import Control.Precondition

import Data.List as L hiding (inits)
import Data.List.Ordered as OL
import Data.Map  as M hiding ((!))
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

import TextShow (showt)
import Text.Printf.TH as Printf

data Program = 
        Event [Expr] Expr Expr EventId
            -- Precondition
            -- wait condition
            -- Conditional execution
            -- event id
        | NotEvent [Expr] [EventId]
            -- Precondition
            -- List of events
        | Wait [Expr] Expr
            -- wait until ...
        | Conditional [Expr] [(Expr, Program)] Program
            -- Precondition, list of branches
        | Sequence          [Program]
            -- list of programs
        | Loop    Expr [Expr] Program Termination
            -- Exit Invariant Body Termination
    deriving (Show)

newtype Partition = Partition [([EventId],Expr)]

newtype MultiProgram = MultiProgram [Program]

type ProgramMaker = RWS RawMachineAST [Program] [Name]

seqP :: [Program] -> Program
seqP [x] = x
seqP xs  = Sequence $ xs

loop :: Expr -> ProgramMaker () -> ProgramMaker ()
loop term body = do
    xs <- snd <$> censor (const []) (listen body)
    let prog = seqP xs
        pre  = precondition prog
    tell [Loop term pre (seqP xs) Infinite]

variant :: Variant -> EventId -> ProgramMaker () -> ProgramMaker ()
variant v evt cmd = censor (L.map f) cmd
    where
        f (Loop term inv body _) = Loop term inv body (Variant v evt) 
        f x = x

natVariant :: Expr -> EventId -> ProgramMaker () -> ProgramMaker ()
natVariant var evt cmd = do
    xs <- get
    put (tail xs)
    variant (IntegerVariant (Var (head xs) int) var (zint 0) Down) evt cmd 

runProgramMaker :: RawMachineAST -> ProgramMaker () -> Program
runProgramMaker m cmd = seqP w
    where
        (_,w) = execRWS cmd m [ makeName $ "var" <> showt i | i <- [0 :: Int ..] ]

wait :: Expr -> ProgramMaker ()
wait e = tell [Wait [] e]

make_multiprogram :: RawMachineAST -> Partition -> MultiProgram 
make_multiprogram m (Partition xs) = MultiProgram $ L.map prog xs
    where
        scheds ls = concatMap (M.elems . view (new.coarse_sched) . (nonSkipUpwards m !)) ls
        prog (ls,term) = runProgramMaker m $ loop term $
            wait $ term `zor` zsome (scheds ls)

data Termination = Infinite | Variant Variant EventId
    deriving (Show)

data Concurrency = Concurrent (Map Name ()) | Sequential

shared_vars :: Concurrency -> Map Name ()
shared_vars (Concurrent s) = s
shared_vars Sequential     = M.empty

atomically' :: (Text -> M a) 
            -> (forall a. (Text -> M a) -> M a)
            -> M a
atomically' f cmd = do
    conc <- asks snd
    let e_name = "expr"
    case conc of
        Concurrent _ -> do
            emit $ [st|%s <- lift $ atomically $ do|] e_name
            indent 2 (cmd $ emit . [st|return %s|])
            f e_name
        Sequential -> do
            -- emit $ [st|%s <- do|] e_name
            cmd f
    -- return e_name        

atomically :: M Text -> M Text
atomically cmd = atomically' return $ \f -> cmd >>= f
    -- conc <- asks snd
    -- case conc of
    --     Concurrent _ -> do
    --         let e_name = "expr"
    --         emit $ [st|%s <- lift $ atomically $ do|] e_name
    --         indent 2 $ do
    --             e <- cmd
    --             emit $ [st|return %s|] e
    --         return e_name
    --     Sequential -> cmd

evaluate :: RawMachineAST -> Expr -> M Text
evaluate m e = head <$> evaluate_all m [e]

evaluate_all :: RawMachineAST -> [Expr] -> M [Text]
evaluate_all m es = do
    conc <- asks snd
    case conc of
        Concurrent _ -> do
            runEval $ (mapM (eval_expr m) es :: ConcurrentEval [Text])
        Sequential -> lift $ mapM (eval_expr m) es

type M = RWST (Int,Concurrency) [Text] () (Either Text)

precondition :: Program -> [Expr]
precondition (Event pre _wait _cond _evt) = pre
precondition (NotEvent pre _)             = pre
        -- L.map ((zall $ cond:wait) `zimplies`) $
        --                                     M.elems (guards $ events m ! evt)
precondition (Wait pre _)        = pre
precondition (Conditional pre _ _) = pre
precondition (Sequence (x:_))    = precondition x
precondition (Sequence [])       = []
precondition (Loop _ inv _ _)    = inv
-- precondition _ (InfLoop _ inv _)   = inv

possibly :: Program -> [EventId]
possibly (Event _ _ _ lbl)  = [lbl]
possibly (NotEvent _ _)     = []
possibly (Wait _ _)         = []
possibly (Conditional _ lb rb) = L.concatMap (possibly . snd) lb ++ possibly rb
possibly (Loop _ _ body _)  = possibly body
possibly (Sequence xs)      = concatMap possibly xs

certainly :: Program -> [EventId]
certainly (Event _ _ _ lbl) = [lbl]
certainly (NotEvent _ evts) = evts
certainly (Wait _ _)        = []
certainly (Sequence xs)     = concatMap certainly xs
certainly (Conditional _ lb rb) = L.foldl' isect (nubSort $ certainly rb) $ L.map nubSort (L.map (certainly . snd) lb)
certainly (Loop _ _ _ _)    = []

safety :: RawMachineAST -> [EventId] -> [Expr] -> Program -> Either [Text] (Map Label Sequent)
safety m others post cfg 
        | L.null es = Right r
        | otherwise = Left es
    where
        (r,(),es) = runRWS (PG.eval_generatorT 
            $ PG.with (do
                    PG._context $ assert_ctx m
                    PG._context $ theory_ctx (m!.theory)
                    PG.named_hyps $ invariants m
                    PG.prefix_label $ as_label $ _name m
                ) $ do
                    PG.with (PG.named_hyps $ m!.inits) $ 
                        establish_pre "init" [] cfg
                    safety_aux cfg post
                ) (DCtx m others $ nubSort $ possibly cfg) ()

establish_pre :: Text -> [Expr] -> Program -> POGen ()
establish_pre prefix ps cfg = 
    PG.with (do
            PG.nameless_hyps ps
            PG.prefix prefix) $
        zipWithM_  (\l p -> PG.emit_goal [label $ showt (l :: Int)] p) 
                [0 :: Int ..] (precondition cfg) 

type POGen = PG.POGenT (RWS DistrContext [Text] ())

data DistrContext = DCtx
        { machine   :: RawMachineAST
        , otherEvts :: [EventId]
        , localEvts :: [EventId]
        }

prefix :: Text -> POGen () -> POGen ()
prefix pre = PG.with $ PG.prefix pre

safety_aux :: Program -> [Expr] -> POGen ()
safety_aux (Event pre wait cond evt_lbl) ps = do
    m <- lift $ asks machine
    others <- lift $ asks otherEvts
    local  <- lift $ asks localEvts
    let evt = all_upwards m ! Right evt_lbl
        sch = evt^.new.coarse_sched
    is_stable_in pre others
    hoare_triple ("postcondition" </> as_label evt_lbl) 
        (cond:wait:pre) evt_lbl ps 
    entails "skip" (znot cond : wait : pre) ps
    entails "forced" (pre ++ M.elems sch) [cond]
    forM_ local $ disabled (znot wait : pre)
safety_aux (NotEvent pre evts) ps = do
    mapM_ (disabled pre) evts
    entails "postcondition" pre ps
safety_aux (Wait pre cond) ps = do
    others <- lift $ asks otherEvts
    is_stable_in pre others
    local  <- lift $ asks localEvts
    mapM_ (disabled $ znot cond:pre) local
    entails "postcondition" (cond : pre) ps
    -- disabled
    -- postcondition
safety_aux (Sequence xs) ps = do
    let prefix
            | L.null $ drop 1 xs = const $ return ()
            | otherwise          = PG.prefix
        prog (l,cfg) post = do
            PG.with (prefix $ showt l) $ do
                safety_aux cfg post
    zipWithM_ prog 
        (zip [0 :: Int ..] xs) 
        (drop 1 $ L.map precondition xs ++ [ps])    
safety_aux (Conditional p xs x) ps = do
    -- PG.with (PG.nameless_hyps p) $ do
    --     PG.emit_goal ["completeness"] $ zsome $ L.map fst xs
    let ys = reverse xs
        bs = zip ((ztrue,x):ys) $ tails $ L.map fst ys
        zs = L.map f bs
        f ((c,b),cs) = (zall $ c:L.map znot cs,b)
    forM_ (zip [0 :: Int ..] zs) $ \(i,(g,b)) -> do
        PG.with (PG.prefix $ "branch" <> showt i) $ do
            establish_pre "precondition" (g:p) b
            safety_aux b ps
safety_aux (Loop exit inv b _) ps = do
    local <- lift $ asks localEvts
    establish_pre "precondition" 
        (znot exit : inv) b
    PG.with (PG.prefix "body")
        $ safety_aux b inv
    entails "postcondition" (exit : inv) ps
    let cert = certainly b
    unless (local == cert)
        $ lift $ tell [[st|Loop is missing events %s|] 
            $ T.intercalate "," $ L.map prettyText $ local L.\\ cert]

is_stable_in :: [Expr] -> [EventId] -> POGen ()
is_stable_in ps evts = do
    -- others <- lift $ asks $ fst . snd
    forM_ evts $ \lbl -> hoare_triple "stable" ps lbl ps

disabled :: [Expr] -> EventId -> POGen ()
disabled ps lbl = do
    evts <- lift $ asks $ upward_event <$> machine <*> pure (Right lbl)
    entails ("disabled" </> as_label lbl) ps 
        [znot $ zall $ evts^.new.coarse_sched]

entails :: Label -> [Expr] -> [Expr] -> POGen ()
entails lbl pre post = do
    let suff i
            | L.null $ drop 1 post = lbl
            | otherwise            = label $ prettyText lbl <> "-" <> showt i
    PG.with (do
            PG.nameless_hyps pre) $ do
        forM_ (zip [0 :: Int ..] post) $ \(i,p) -> 
            PG.emit_goal [suff i] p

hoare_triple :: Label -> [Expr] -> EventId -> [Expr] -> POGen ()
hoare_triple lbl pre evt_lbl post = do
    m <- lift $ asks machine
    let evt = upward_event m (Right evt_lbl)
        grd = evt^.new.guards
        act = ba_predicate m evt
    PG.with (do 
            PG._context $ step_ctx m
            PG.named_hyps $ grd
            PG.named_hyps $ act
            PG.variables $ evt^.new.indices
            PG.variables $ evt^.new.params) $ do
        entails lbl pre post

default_cfg :: RawMachineAST -> Program
default_cfg m = Loop g [] body Infinite
    where
        all_guard e = zall $ e^.new.coarse_sched
        g    = zsome xs
        xs   = L.map (znot . all_guard) $ M.elems $ nonSkipUpwards m
        branch (Right lbl,e) = [Event [] ztrue (all_guard e) lbl]
        branch _ = []
        body = Sequence 
            $ concatMap branch
            $ M.toList $ all_upwards m

emit :: Text -> M ()
emit xs = do
    n <- asks fst
    forM_ (T.lines xs) $ \ln -> 
        tell [T.replicate n " " <> ln]

emitAll :: [Text] -> M ()
emitAll = mapM_ emit

indent :: Int -> M a -> M a
indent n = local $ first (n+)

type_code :: Type -> Either Text Text
type_code t = 
            case t of
                Gen s []
                    | s == IntSort ->  return "Int"
                    | s == BoolSort -> return "Bool"
                Gen s [t]
                    | s == set_sort -> do
                        c <- type_code t
                        return $ [Printf.st|S.Set (%s)|] c
                Gen s [t0,t1]
                    | s == fun_sort -> do
                        c0 <- type_code t0
                        c1 <- type_code t1
                        return $ [Printf.st|M.Map (%s) (%s)|] c0 c1
                _ -> Left $ [st|unrecognized type: %s|] (prettyText t)
                    
binops_code :: Map Name (Text -> Text -> Text)
binops_code = M.fromList 
    [ (z3Name "=", [st|(%s == %s)|])
    , (z3Name "+", [st|(%s + %s)|])
    , (z3Name "<", [st|(%s < %s)|])
    , (z3Name "ovl", flip $ [st|(M.union %s %s)|])
    , (z3Name "mk-fun", [st|(M.singleton %s %s)|])
    ]

unops_code :: Map Name (Text -> Text)
unops_code = M.fromList
    [ (z3Name "not", [st|(not %s)|])]

nullops_code :: Map Name Text
nullops_code = M.fromList
    [ (z3Name "empty-fun", "M.empty") 
    , (z3Name "empty-set", "S.empty")]

class Monad m => Evaluator m where
    read_var :: Name -> m ()
    left :: Text -> m a
    is_shared :: Name -> m Bool

instance Evaluator (Either Text) where
    left = Left
    read_var _ = return ()
    is_shared _ = return False

type ConcurrentEval = RWST (Map Name ()) [Name] () (Either Text)

instance Evaluator ConcurrentEval where
    is_shared v = do
        sv <- ask
        return (v `M.member` sv)
    read_var v = do
        b <- is_shared v
        when b $ tell [v]
    left = lift . Left

addPrefix :: Pre
          => Text -> Name -> Name
addPrefix pre n = fromText $ pre <> renderText n 

eval_expr :: Evaluator m => RawMachineAST -> Expr -> m Text
eval_expr m e =
        case e of
            Word (Var n _)
                | n `M.member` view' variables m -> do
                    read_var n
                    return $ "v_" <> renderText n
                | otherwise              -> return $ "c_" <> renderText n
            Lit n _    -> return $ prettyText n
            FunApp f [] 
                | view name f `M.member` nullops_code -> return $ nullops_code ! view name f
            FunApp f0 [e0,FunApp f1 [e1,e2]] 
                | view name f0 == z3Name "ovl" && view name f1 == z3Name "mk-fun" -> do
                    c0 <- eval_expr m e0
                    c1 <- eval_expr m e1
                    c2 <- eval_expr m e2
                    return $ [st|(M.insert %s %s %s)|] c1 c2 c0
            FunApp f [e]
                | view name f `M.member` unops_code -> do
                    c <- eval_expr m e
                    return $ (unops_code ! view name f) c
            FunApp f [e0,e1] 
                | view name f `M.member` binops_code -> do
                    c0 <- eval_expr m e0
                    c1 <- eval_expr m e1
                    return $ (binops_code ! view name f) c0 c1
            _ -> left $ [st|unrecognized expression: %s|] (prettyText e)

struct :: RawMachineAST -> M ()
struct m = do
        sv <- asks (shared_vars . snd)
        let attr :: (Map Name Var -> Map Name () -> Map Name Var)
                 -> Text -> (Text -> Text)
                 -> Either Text Text
            attr comb pre typef = do 
                code <- mapM (decl typef) $ 
                               L.map (pre,) (M.elems $ view' variables m `comb` sv) 
                return $ T.intercalate "\n    , " code
            s_attr = attr M.intersection "s" ([st|TVar (%s)|] :: Text -> Text)
            l_attr = attr M.difference "v" id
            decl :: (Text -> Text) -> (Text,Var) -> Either Text Text
            decl typef (pre,Var y t) = do
                code <- type_code t
                return $ [st|%s_%s :: %s|] (pre :: Text) (renderText y) (typef code)
        unless (M.null sv) $ do
            code <- lift $ s_attr
            emit $ "data Shared = Shared\n    { " <> code <> " }"
            emit "\n"
        code <- lift $ l_attr
        emit $ "data State = State\n    { " <> code <> " }"

assign_code :: RawMachineAST -> RawAction -> ConcurrentEval (Bool,Text)
assign_code m (Assign v e) = do
        c0 <- eval_expr m e
        b <- is_shared $ v^.name
        -- sv <- asks $ shared_vars . snd
        -- let b = name v `M.member` sv
        return $ (b,if b 
            then [st|writeTVar s_%s %s|] (renderText $ v^.name) c0
            else [st|v_%s = %s|] (renderText $ v^.name) c0)
assign_code _ act@(BcmSuchThat _ _) = left $ [st|Action is non deterministic: %s|] (prettyText act)
assign_code _ act@(BcmIn _ _) = left $ [st|Action is non deterministic: %s|] (prettyText act)

init_value_code :: Evaluator m => RawMachineAST -> Expr -> m [(Bool,(Name,Text))]
init_value_code m e =
        case e of
            FunApp f [Word (Var n _),e0]
                    |      n `M.member` (m!.variables)
                        && view name f == z3Name "=" -> do
                                b  <- is_shared n
                                c0 <- eval_expr m e0
                                return [(b,(n,c0))]
            FunApp f es
                    | view name f == z3Name "and" -> do
                        rs <- mapM (init_value_code m) es
                        return $ concat rs
            _ -> left $ [st|initialization is not in a canonical form: %s|] (prettyText e)

runEval :: ConcurrentEval a -> M a
runEval cmd = do
    sv <- asks $ shared_vars . snd
    (e,(),rs) <- lift $ runRWST cmd sv ()
    forM_ (nubSort rs) $ \r -> 
        emit $ [st|v_%s <- readTVar s_%s|] (renderText r) (renderText r)
    return e

event_body_code :: RawMachineAST -> RawEvent -> M Text
event_body_code m e = do
        acts <- runEval $ mapM (assign_code m) $ M.elems $ e^.actions
        -- evaluate_all 
        let (g_acts,l_acts) = (L.map snd *** L.map snd) $ L.partition fst acts
        emit "let s' = s"
        indent 8 $ do
            case l_acts of 
                x:xs -> do
                    emit $ [st|{ %s|] x
                    mapM_ (emit . (", " <>)) xs
                    emit "}"
                []   -> emit "s'"
        emitAll g_acts
        return "s'"
        -- emit "modify $ \\s'@(State { .. }) ->"
        -- indent 2 $ do
        --     case acts of 
        --         x:xs -> do
        --             emit $ [st|s' { %s|] x
        --             indent 3 $ do
        --                 mapM_ (emit . (", " ++)) xs
        --                 emit "}"
        --         []   -> emit "s'"

report :: Text -> M a
report = lift . Left

-- event_code :: MachineAST -> UB.Event -> M ()
-- event_code m e = do
--         unless (M.null $ params e) $ report "non null number of parameters"
--         unless (M.null $ indices e) $ report "non null number of indices"
--         unless (isNothing $ fine $ new_sched e) $ report "event has a fine schedule"
--         grd  <- lift $ eval_expr m $ zall $ coarse $ new_sched e
--         emit $ [st|if %s then|] grd
--         indent 2 $ event_body_code m e
--         emit $ "else return ()"

conc_init_code :: RawMachineAST -> M ()
conc_init_code m = do
        acts' <- runEval $ liftM concat 
            $ mapM (init_value_code m) $ M.elems $ m!.inits
        let acts = L.map snd $ L.filter fst acts' 
        emitAll $ L.map (\(v,e) -> [st|s_%s <- newTVarIO %s|] (prettyText v) e) acts

init_code :: RawMachineAST -> M ()
init_code m = do
        acts' <- runEval $ liftM concat 
            $ mapM (init_value_code m) $ M.elems $ m!.inits
        let acts = L.map snd $ L.filter (not . fst) acts' 
        emit "s' = State"
        indent 5 $ do
            emitAll $ zipWith (<>) ("{ ":repeat ", ") $ L.map (uncurry $ [st|v_%s = %s|] . renderText) acts
            when (not $ L.null acts) 
                $ emit "}"

if_concurrent :: M () -> M ()
if_concurrent cmd = do
        conc <- asks snd
        case conc of
          Sequential -> return ()
          Concurrent _ -> cmd


write_seq_code :: RawMachineAST -> Program -> M ()
write_seq_code m (Event _pre wait cond lbl)          
    | wait == ztrue = do
        emit "s@(State { .. }) <- get"
        if_concurrent $ emit "(Shared { .. }) <- ask"
        let f = emit . [st|put %s|]
        atomically' f $ \f -> do
            expr <- evaluate m cond
            emit $ [st|if %s then do|] expr
            indent 2 $ do
                s' <- event_body_code m (upward_event m (Right lbl)^.new)
                _  <- f s'
                return ()
            emit $ [st|else|]    
            indent 2 $ f "s"
    | otherwise = lift $ Left "Waiting is not allowed in sequential code"
write_seq_code _ (NotEvent _ _) = return ()
write_seq_code _ (Wait _ _)     = lift $ Left "Waiting is not allowed in sequential code"
write_seq_code m (Conditional _ ((c,b):cs) eb) = do
    emit "(State { .. }) <- get"
    if_concurrent $ emit "(Shared { .. }) <- ask"
    expr <- atomically $ evaluate m c
    emit $ [st|if %s then do|] expr
    indent 2 $ write_seq_code m b
    forM_ cs $ \(c,b) -> do
        expr <- lift $ eval_expr m c
        emit $ [st|else if %s then do|] expr
        indent 2 $ write_seq_code m b
    emit $ "else do"
    indent 2 $ write_seq_code m eb
write_seq_code m (Conditional _ [] eb) = write_seq_code m eb
write_seq_code m (Sequence xs) = do
    forM_ xs $ \b -> do
        write_seq_code m b
write_seq_code m (Loop exit _inv b _) = do
    emit "fix $ \\proc' -> do"
    indent 2 $ do
        emit "(State { .. }) <- get"
        if_concurrent $ emit "(Shared { .. }) <- ask"
        exitc <- atomically $ evaluate m exit
        emit $ [st|if %s then return ()|] exitc
        emit "else do"
        indent 2 $ do
            write_seq_code m b
            emit "proc'"
-- emit "(State { .. }) <- get"
--             exitc <- eval_expr m exit
--             emit $ [st|if %s then return ()|] exitc
--             emit "else do"
--             indent 2 $ do
--                 mapM (event_code m) $ M.elems $ events m
--                 emit "proc'"

machine_code :: Text -> RawMachineAST -> Expr -> M ()
machine_code name m _exit = do
        x <- asks snd
        let args = foldMap ((" c_" <>).renderText) $ M.keys $ m!.theory.consts
            cfg  = default_cfg m
            trans :: Text -> Text -> Text
            trans = case x of
                     Sequential -> [st|execState %s %s|]
                     Concurrent _ -> [st|fst `liftM` (execRWST %s (Shared { .. }) %s :: IO (Main.State,()))|]
        emit $ [st|%s%s = do|] name args
        indent 8 $ do
            conc_init_code m
            emit $ trans "proc" "s'" 
        indent 4 $ do
            emit "where"
            indent 4 $ do
                init_code m
                emit "proc ="
                indent 7 $ write_seq_code m cfg

run' :: Concurrency -> M () -> Either Text Text
run' c cmd = liftM (T.unlines . snd) $ execRWST cmd (0,c) ()

run :: M () -> Either Text Text
run = run' Sequential

source_file :: Text -> RawMachineAST -> Expr -> Either Text Text
source_file = source_file' []

source_file' :: [Name] -> Text -> RawMachineAST -> Expr -> Either Text Text
source_file' shared name m exit = 
        run' c $ do
            emitAll $
                [ "{-# LANGUAGE RecordWildCards #-}"
                , "import Data.Map as M"
                , "import Data.Set as S" ] ++
                imp ++
                [ "import Control.Monad"
                , "import Control.Monad.Fix"
                , "import Control.Monad.State.Class"
                , "import Control.Monad.Trans"
                , "import Control.Monad.Trans.RWS   hiding (get,put)"
                , "import Control.Monad.Trans.State hiding (get,put)"
                , "\n"
                ]
            struct m
            emit "\n"
            machine_code name m exit
    where
        c 
            | L.null shared = Sequential
            | otherwise     = Concurrent $ M.fromList $ zip shared $ repeat ()
        imp 
            | L.null shared = []
            | otherwise     = ["import Control.Concurrent.STM"]
