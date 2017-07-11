{-# LANGUAGE ViewPatterns 
    , OverloadedStrings
    #-}
module Documentation.SummaryGen where

    -- Modules
import Language.UnitB as UB
import Language.UnitB.Expr   hiding ((</>))

    -- Libraries
import Control.Applicative
import Control.Lens hiding ((<.>),indices)

import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Trans.Reader (ReaderT(..),runReaderT)
import Control.Precondition

import Data.Default
import qualified Data.List as L
import           Data.List.NonEmpty as NE hiding ((!!))
import           Data.Map as M hiding (map)
import qualified Data.Map as M
import           Data.Maybe as MM
import           Data.Text (Text)
import qualified Data.Text as T

import System.FilePath
import System.IO.FileSystem

import Prelude hiding (writeFile,readFile)

import Text.Printf.TH

newtype Doc a = Doc { getDoc :: ReaderT FilePath FileSystemM a }
    deriving (Functor)
    -- Reader parameters:
    --      AST -> LaTeX conversions
    --      path for listing creation
type M = RWS Bool [Text] ()
    --      AST -> LaTeX conversions
    --      should we strike out expressions?

data ExprStyle = Tagged | Untagged | Definition

data ExprDispOpt label expr = ExprDispOpt
        { _makeDoc :: label -> M ()                 -- Command to produce documentating comments
        , _style :: ExprStyle
        , _pretty' :: label -> Text
        , _isDefaultSpecial :: Maybe (label -> Label -> Bool)
        , _makeString :: expr -> Text           -- How to convert (AST) type `a` to LaTeX?
        }                                                      -- 

makeLenses ''ExprDispOpt

isDefault :: ExprDispOpt label expr -> label -> Bool
isDefault p lbl = maybe False (\f -> f lbl "default") 
                        (p^.isDefaultSpecial)

makeRef :: ExprDispOpt label expr 
        -> EventId -> label -> M Text
makeRef p pre lbl
    | isDefault p lbl = return $ [st|(\\ref{%s}/default)|] $ prettyText pre
    | otherwise       = return $ [st|\\eqref{%s}|] (prettyText pre <> (p^.pretty') lbl)

combineLblExpr :: ExprDispOpt label expr
               -> EventId -> label -> Text -> M Text
combineLblExpr opts pre lbl expr = case opts^.style of 
                    Untagged   -> [st|  \\item[ ]%s\n  %%%s|] 
                                            -- <$> makeRef opts pre lbl
                                            <$> format_formula expr
                                            <*> makeRef opts pre lbl
                    Tagged     -> [st|  \\item[ %s ]%s|] 
                                            <$> makeRef opts pre lbl
                                            <*> format_formula expr
                    Definition -> fmap ("  \\item[] " <>)
                                    $ format_formula 
                                    $ [st|%s \\3\\triangleq %s|] 
                                            (opts^.pretty' $ lbl) 
                                            expr

instance Applicative Doc where
    pure x = Doc $ pure x
    Doc f <*> Doc x = Doc $ f <*> x

instance Monad Doc where
    Doc m >>= f = Doc $ m >>= getDoc . f
instance MonadReader FilePath Doc where
    ask = Doc ask
    local f (Doc m) = Doc $ local f m
instance FileSystem Doc where
    liftFS f = NoParam $ Doc $ lift $ getNoParam f
    lift2FS f = OneParam $ \g -> Doc $ ReaderT $ \fn -> getOneParam f $ \x -> runReaderT (getDoc $ g x) fn

defOptions :: PrettyPrintable label 
           => (expr -> Text) 
           -> ExprDispOpt label expr
defOptions f = ExprDispOpt
            { _makeDoc = const $ return () 
            , _makeString = f 
            , _style = Untagged
            , _pretty' = prettyText
            , _isDefaultSpecial = Nothing
            }

instance PrettyPrintable label => Default (ExprDispOpt label Expr) where
    def = defOptions prettyPrint

show_removals :: Bool
show_removals = True

runDoc :: Doc a -> FilePath -> FS a
runDoc (Doc cmd) = unFileSystemM . runReaderT cmd

produce_summaries :: FilePath -> System -> FS ()
produce_summaries path sys = do
        createDirectoryIfMissing True path'
        runDoc (do
            forM_ (M.elems ms) $ \m -> do
                machine_summary sys m
                properties_summary m
                forM_ (M.toAscList $ nonSkipUpwards m) $ \(lbl,evt) -> do
                    event_summary m lbl evt
            ) path
    where
        ms = sys!.machines
        path' = dropExtension path

make_file :: FilePath -> M () -> Doc ()
make_file fn cmd = do
    path <- ask
    let xs = snd $ execRWS cmd False ()
        root = [st|%%!TEX root=../%s|] (takeFileName path)
        
        -- this is not writeFile from Prelude, it comes
        -- from filesystem-mockup
    writeFile (dropExtension path </> fn) 
        $ T.unlines $ root : xs

keyword :: Text -> Text
keyword kw = [st|\\textbf{%s}|] kw

machine_summary :: System -> Machine -> Doc ()
machine_summary sys m = do
    path <- ask
    make_file fn $ 
        block $ do
            item $ tell [keyword "machine" <> " " <> renderText (m!.name)]
            item $ refines_clause sys m
            item $ set_sum m
            item $ constant_sum m
            unless (M.null $ m!.theory.fact)
                $ item $ input path $ asm_file m
            item $ variable_sum m
            unless (M.null $ m!.defs)
                $ item $ input path $ defs_file m
            unless (M.null $ _inv $ m!.props) 
                $ item $ input path $ inv_file m
            unless (M.null $ _inv_thm $ m!.props) 
                $ item $ input path $ inv_thm_file m
            unless (M.null $ _progress $ m!.props) 
                $ item $ input path $ live_file m
            unless (M.null $ _safety $ m!.props) 
                $ item $ input path $ saf_file m
            unless (M.null $ _transient $ m!.props)
                $ item $ input path $ transient_file m
            unless (M.null $ _constraint $ m!.props)
                $ item $ input path $ constraint_file m
            item $ do
                tell [keyword "initialization"]
                init_summary m
                tell [keyword "events"]
                let evts = keys $ nonSkipUpwards m
                unless (L.null evts) $
                    block $ forM_ evts $ \k -> do
                        item $ input path $ event_file_name m k
            item $ kw_end
    where
        fn = "machine_" ++ render (m!.name) <.> "tex"

prop_file_name :: Machine -> FilePath
prop_file_name m = "machine_" ++ render (m!.name) ++ "_props" <.> "tex"

indent :: Int -> Text -> Text
indent n xs = T.intercalate "\n" $ L.map (margin <>) $ T.lines xs
    where
        margin = T.replicate n " "

item :: M () -> M ()
item cmd = do
    let f [] = []
        f xs = ["  \\item " <> T.intercalate "\n" (L.map (indent 2) xs)]
    censor f cmd

properties_summary :: Machine -> Doc ()
properties_summary m = do
        let prop = m!.props
        path <- ask
        make_file (asm_file m) $ asm_sum m
        make_file (defs_file m) $ defs_sum m
        make_file (inv_file m) $ invariant_sum m
        make_file (inv_thm_file m) $ invariant_thm_sum prop
        make_file (live_file m) $ liveness_sum m
        make_file (transient_file m) $ transient_sum m
        make_file (saf_file m) $ safety_sum prop
        make_file (constraint_file m) $ constraint_sum m
        make_file fn $ block $ do
            item $ input path $ asm_file m
            item $ input path $ defs_file m
            item $ input path $ inv_file m
            item $ input path $ inv_thm_file m
            item $ input path $ live_file m
            item $ input path $ saf_file m
            item $ input path $ transient_file m
            item $ input path $ constraint_file m
    where
        fn = prop_file_name m

asm_file :: Machine -> FilePath
asm_file m  = "machine_" ++ render (m!.name) ++ "_asm" <.> "tex"

defs_file :: Machine -> FilePath
defs_file m  = "machine_" ++ render (m!.name) ++ "_def" <.> "tex"

inv_file :: Machine -> FilePath
inv_file m  = "machine_" ++ render (m!.name) ++ "_inv" <.> "tex"

inv_thm_file :: Machine -> FilePath
inv_thm_file m  = "machine_" ++ render (m!.name) ++ "_thm" <.> "tex"

live_file :: Machine -> FilePath
live_file m = "machine_" <> render (m!.name) <> "_prog" <.> "tex"

transient_file :: Machine -> FilePath
transient_file m = "machine_" <> render (m!.name) <> "_trans" <.> "tex"

saf_file :: Machine -> FilePath
saf_file m  = "machine_" <> render (m!.name) <> "_saf" <.> "tex"

constraint_file :: Machine -> FilePath
constraint_file m  = "machine_" <> render (m!.name) <> "_co" <.> "tex"

getListing :: M () -> Text
getListing cmd = T.unlines $ snd $ execRWS cmd False ()

input :: FilePath -> FilePath -> M ()
input path fn = do
    tell [[st|\\input{%s/%s}|] (takeBaseName path) $ dropExtension fn]

kw_end :: M ()
kw_end = tell ["\\textbf{end} \\\\"]

event_file_name :: Machine -> EventId -> FilePath
event_file_name m lbl = (render (m!.name) ++ "_" ++ reformat lbl) <.> "tex"
    where
        reformat lbl = L.map f $ pretty lbl
        f ':' = '-'
        f x   = x

comment_of :: Machine -> DocItem -> M ()
comment_of m key = do
    item $ do
        case key `M.lookup` (m!.comments) of
            Just cmt -> block $ item $ tell [[st|%s|] cmt]
            Nothing -> return ()

init_summary :: Machine -> M ()
init_summary m = do
    when show_removals $
        local (const True)
            $ put_all_expr "" $ M.toAscList $ m!.del_inits
    put_all_expr "" $ M.toAscList $ m!.UB.inits

event_summary' :: Machine -> EventId -> EventMerging' -> M ()
event_summary' m lbl e = do
        index_sum lbl e
        comment_of m (DocEvent lbl)
        block $ do
            item $ refines_sum e
            item $ csched_sum lbl e
            item $ fsched_sum lbl e
            item $ param_sum e
            item $ guard_sum lbl e
            item $ act_sum lbl e
            item $ kw_end

event_summary :: Machine -> EventId -> EventMerging' -> Doc ()
event_summary m lbl e = make_file fn $ 
        event_summary' m lbl e
    where
        fn = event_file_name m lbl

refines_clause :: System -> Machine -> M ()
refines_clause sys m = do
    case join $ _name m `M.lookup` (sys!.ref_struct) of
        Nothing -> return ()
        Just anc -> tell [keyword "refines" <> " " <> prettyText anc]

block :: M () -> M ()
block cmd = do
    xs <- censor (const []) $ snd <$> listen cmd
    unless (L.null xs) $ do
        tell ["\\begin{block}"]
        tell xs
        tell ["\\end{block}"]

set_sum :: Machine -> M ()
set_sum m = section (keyword "sets") $ 
    unless (M.null $ m!.theory.types) $
    block $ do
        forM_ (keys $ m!.theory.types) $ \v -> do
            item $ tell [[st|$%s$|] $ renderText v]

constant_sum :: Machine -> M ()
constant_sum m = section (keyword "constants") $ 
    unless (M.null $ m!.theory.consts) $
    block $ do
        forM_ (elems $ m!.theory.consts) $ \v -> do
            item $ tell [[st|$%s$|] $ varDecl v]

variable_sum :: Machine -> M ()
variable_sum m = section (keyword "variables") $ 
    unless (M.null (m!.variables) && M.null (m!.abs_vars)) $
    block $ do
        when show_removals $ 
            forM_ (elems $ view' abs_vars m `M.difference` view' variables m) $ \v -> do
                item $ tell [[st|$%s$\\quad(removed)|] $ varDecl v]
                comment_of m (DocVar $ v^.name)
        forM_ (elems $ view' abs_vars m `M.intersection` view' variables m) $ \v -> do
            item $ tell [[st|$%s$|] $ varDecl v]
            comment_of m (DocVar $ v^.name)
        forM_ (elems $ view' variables m `M.difference` view' abs_vars m) $ \v -> do
            item $ tell [[st|$%s$\\quad(new)|] $ varDecl v]
            comment_of m (DocVar $ v^.name)

data Member = Elem | Subset

varDecl :: Var -> Text
varDecl v = renderText (v^.name) <> fromMaybe "" (isMember $ type_of v)

withParenth :: Bool -> Text -> Text
withParenth True  = [st|(%s)|]
withParenth False = id

isMember :: Type -> Maybe Text
isMember t = join (preview (_FromSort.to f) t) <|> ((" \\in " <>) <$> typeToSet False t)
    where
        f (DefSort n _ _ _,ts) 
            | n == [tex|\set|] = [st| \\subseteq %s|] <$> typeToSet False (ts !! 0)
        f _ = Nothing


typeToSet :: Bool -> Type -> Maybe Text
typeToSet paren = join . preview (_FromSort.to f)
    where
        f (DefSort n _ _ _,ts) 
            | n == [tex|\set|] = withParenth paren . [st|\\pow.%s|] <$> typeToSet True (ts !! 0)
            | n == [tex|\pfun|] = withParenth paren <$> 
                                    liftM2 [st|%s \\pfun %s|] 
                                        (typeToSet True (ts !! 0))
                                        (typeToSet True (ts !! 1))
        f (Sort n _ _,_) 
            | otherwise  = Just (renderText n)
        f (IntSort,[]) = Just [st|\\mathbb{Z}|]
        f (RealSort,[]) = Just [st|\\mathbb{R}|]
        f (BoolSort,[]) = Just [st|\\textbf{Bool}|]
        f _ = Nothing


asm_sum :: Machine -> M ()
asm_sum m = do
        let props = m!.theory.fact
        section kw_inv $ put_all_expr_with 
                (style .= Tagged) 
                "" (M.toAscList $ props) 
    where
        kw_inv = "\\textbf{assumptions}"

defs_sum :: Machine -> M ()
defs_sum m = do
        section kw_defs $ put_all_expr_with'
                prettyPrint 
                (style .= Definition) 
                "" (M.toAscList $ m!.defs) 
    where
        kw_defs = "\\textbf{definitions}"

invariant_sum :: Machine -> M ()
invariant_sum m = do
        let prop = m!.props
        section kw_inv $ put_all_expr_with 
                (do makeDoc .= comment_of m . DocInv
                    style .= Tagged) 
                "" (M.toAscList $ prop^.inv) 
    where
        kw_inv = "\\textbf{invariants}"
        
invariant_thm_sum :: PropertySet -> M ()
invariant_thm_sum prop = 
        section kw_thm $ put_all_expr "" (M.toAscList $ prop^.inv_thm)
    where
        kw_thm = "\\textbf{invariants} (theorems)"

liveness_sum :: Machine -> M ()
liveness_sum m = do
        let prop = m!.props
        section kw $ put_all_expr_with' toString 
                (style .= Tagged)
                "" (M.toAscList $ prop^.progress) 
    where
        kw = "\\textbf{progress}"
        toString (LeadsTo _ p q) = 
            [st|%s \\quad \\mapsto\\quad %s|]
                (prettyPrint p)
                (prettyPrint q)

safety_sum :: PropertySet -> M ()
safety_sum prop = do
        section kw $ put_all_expr_with' toString 
                (style .= Tagged)
                "" (M.toAscList $ prop^.safety)
    where
        kw = "\\textbf{safety}"
        toString (Unless _ p q) = 
                [st|%s \\textbf{\\quad unless \\quad} %s|] p' q'
            where
                p' = prettyPrint p
                q' = prettyPrint q

transient_sum :: Machine -> M ()
transient_sum m = do
        let prop = m!.props
        section kw $ put_all_expr_with' toString 
                (style .= Tagged)
                "" (M.toAscList $ prop^.transient) 
    where
        kw = "\\textbf{transient}"
        toString (Tr _ p evts hint) = 
                [st|%s \\qquad \\text{(%s$%s$%s)}|] 
                    p' evts' sub'' lt'
            where
                TrHint sub lt = hint
                evts' :: Text
                evts' = T.intercalate "," $ L.map ([st|\\ref{%s}|] . pretty) (NE.toList evts)
                sub'  = M.toList sub & traverse._2 %~ (prettyPrint . snd)
                isNotIdent n (getExpr -> Word (Var n' _)) = n /= n'
                isNotIdent _ _ = True
                sub'' :: Text
                sub'' 
                    | M.null $ M.filterWithKey isNotIdent $ M.map snd sub = ""
                    | otherwise  = [st|: [%s]|] (T.intercalate ", " $ L.map asgnString sub')
                asgnString (v,e) = [st|%s := %s'~|~%s|] (renderText v) (renderText v) e
                lt' :: Text
                lt' = maybe "" ([st|, with \\eqref{%s}|] . pretty) lt
                p' = prettyPrint p

constraint_sum :: Machine -> M ()
constraint_sum m = do
        let prop = m!.props
        section kw $ put_all_expr_with' toString 
                (style .= Tagged)
                "" (M.toAscList $ prop^.constraint)
    where
        kw = "\\textbf{safety}"
        toString (Co _ p) = prettyPrint p

format_formula :: Text -> M Text
format_formula str = do
        sout <- ask    -- Strike out the formula?
        let sout' 
                | sout      = "\\sout"
                | otherwise = ""
        return $ T.intercalate "\n" $ L.map (<> " %") $ T.lines $ sout' <> "{$" <> f str <> "$}"
            -- what is the point of ending the L.lines with '%' ?
    where
        f xs = T.concatMap g xs
        g '&' = ""
        g x = T.singleton x

put_expr :: ExprDispOpt label expr     
         -> EventId             -- label prefix (for LaTeX cross referencing)
         -> (label,expr)        -- AST and its label
         -> M ()
put_expr opts pre (lbl,e) = do
        let expr = opts^.makeString $ e
        tell . pure =<< combineLblExpr opts pre lbl expr
        opts^.makeDoc $ lbl

put_all_expr' :: PrettyPrintable label 
              => (a -> Text) 
              -> EventId 
              -> [(label, a)] -> M ()
put_all_expr' f = put_all_expr_with' f (return ())

put_all_expr_with' :: PrettyPrintable label
                   => (expr -> Text)
                   -> State (ExprDispOpt label expr) ()
                   -> EventId 
                   -> [(label, expr)] -> M ()
put_all_expr_with' toString opts pre xs
    | L.null xs = return ()
    | otherwise = 
        block $ forM_ xs $ put_expr (execState opts $ defOptions toString) pre

put_all_expr :: EventId -> [(Label,Expr)] -> M ()
put_all_expr = put_all_expr' prettyPrint

put_all_expr_with :: State (ExprDispOpt Label Expr) () 
                  -> EventId -> [(Label, Expr)] -> M ()
put_all_expr_with opts = put_all_expr_with' prettyPrint opts

section :: Text -> M () -> M ()
section kw cmd = do
    let f [] = []
        f xs = kw:xs
    censor f cmd

index_sum :: EventId -> EventMerging' -> M ()
index_sum lbl e = tell ["\\noindent \\ref{" <> prettyText lbl <> "} " <> ind <> " \\textbf{event}"]
    where
        ind 
            | M.null $ e^.indices = ""
            | otherwise           = "$[" <> inds <> "]$"
        inds = T.intercalate "," (L.map (renderText . view name) $ M.elems $ e^.indices)

refines_sum :: EventMerging' -> M ()
refines_sum e = do
        section kw $ block $ do
            mapM_ (item.tell.pure.disp) 
                  (MM.mapMaybe (rightToMaybe.fst) $ NE.toList $ e^.multiAbstract)
    where
        kw = "\\textbf{refines}"
        disp :: EventId -> Text
        disp = [st|\\ref{%s}|] . pretty

csched_sum :: EventId -> EventMerging' -> M ()
csched_sum lbl e = do
        -- unless (sch == def) $ 
        section kw $ do
            when show_removals $
                local (const True)
                    $ put_all_expr_with (isDefaultSpecial .= Just (==)) lbl del_sch
            put_all_expr_with (isDefaultSpecial .= Just (==)) lbl sch
    where
        kw = "\\textbf{during}"    
        sch = M.toAscList $ e^.new.coarse_sched --coarse $ new_sched e
        del_sch = concatMap (fmap M.toList $ view $ deleted.coarse_sched) $ e^.evt_pairs.to NE.toList
        -- def = M.singleton (label "default") zfalse

fsched_sum :: EventId -> EventMerging' -> M ()
fsched_sum lbl e = section kw $ do
        when show_removals $
            local (const True)
                $ put_all_expr lbl del_sch
        put_all_expr lbl xs
    where
        kw = "\\textbf{upon}"
        xs = e^.new' fine_sched -- fine $ new_sched e
        del_sch = concatMap (fmap M.toAscList $ view $ deleted.fine_sched) $ e^.evt_pairs.to NE.toList -- fine $ deleted_sched e

param_sum :: EventMerging' -> M ()
param_sum e
    | M.null $ e^.params  = return ()
    | otherwise           = do
        tell [[st|\\textbf{any} $%s$|] $ 
                T.intercalate "," (L.map (renderText . view name) $ M.elems $ e^.params)]

guard_sum :: EventId -> EventMerging' -> M ()
guard_sum lbl e = section kw $ do
        when show_removals $
            local (const True)
                $ put_all_expr lbl $ e^.deleted' raw_guards -- deleted_guard e
        put_all_expr lbl $ e^.new' raw_guards -- new_guard e
    where
        kw = "\\textbf{when}"

act_sum :: EventId -> EventMerging' -> M ()
act_sum lbl e = section kw $ do
        when show_removals $
            local (const True)
                $ put_all_expr' put_assign lbl $ e^.deleted' actions -- del_acts e
        put_all_expr' put_assign lbl $ M.toAscList $ e^.actions
    where 
        kw = "\\textbf{begin}"
        put_assign (Assign v e) = 
            [st|%s \\bcmeq %s|] 
                (renderText $ v^.name)
                (prettyPrint e)
        put_assign (BcmSuchThat vs e) = 
            [st|%s \\bcmsuch %s|] 
                (T.intercalate "," $ L.map (renderText . view name) vs :: Text)
                (prettyPrint e)
        put_assign (BcmIn v e) = 
            [st|%s \\bcmin %s|] 
                (renderText $ v^.name)
                (prettyPrint e)

