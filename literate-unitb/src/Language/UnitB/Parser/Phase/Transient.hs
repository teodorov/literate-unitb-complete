{-# LANGUAGE TypeOperators #-}
module Language.UnitB.Parser.Phase.Transient where

    --
    -- Modules
    --
import Latex.Parser hiding (contents)

import Logic.Expr
import Logic.Expr.Parser

import Language.UnitB.Parser.Phase as P
import Language.UnitB.Parser.Phase.Types
import Language.UnitB.Parser.Visitor
import Language.UnitB.Syntax as AST

    --
    -- Libraries
    --
import           Control.Monad.RWS as RWS ( RWS )
import           Control.Precondition

import Control.Lens as L hiding ((|>),(<.>),(<|),indices,Context)

import Data.Either.Validation
import qualified Data.Maybe as MM
import           Data.List as L hiding ( union, insert, inits )
import qualified Data.List.NonEmpty as NE
import           Data.HashMap.Lazy   as M hiding ( (\\), (!) )
import qualified Data.Text as T

import Text.Printf.TH

import Utilities.Syntactic

tr_hintV :: HasMachineP2 mch
         => mch
         -> HashMap Name Var
         -> NonEmpty EventId
         -> LatexDoc
         -> Either [Error] TrHint
tr_hintV p2 vs evts doc = validationToEither $ eitherToValidation r <* nonNullError es
    where
        (r,es) = runM (tr_hint p2 vs (as_label <$> evts) doc) (line_info doc)

nonNullError :: [e] -> Validation [e] ()
nonNullError [] = pure ()
nonNullError es = Failure es

tr_hint :: HasMachineP2 mch
        => mch
        -> HashMap Name Var
        -> NonEmpty Label
        -> LatexDoc
        -> M TrHint
tr_hint p2 vs lbls thint = do
    tr@(TrHint wit _)  <- toEither $ tr_hint' p2 vs lbls thint empty_hint
    evs <- get_events p2 $ NE.toList lbls
    let vs = L.map (view pIndices p2 !) evs
        err e ind = ( not $ M.null diff
                    , [st|A witness is needed for %s in event '%s'|] 
                        (T.intercalate "," $ renderText <$> keys diff) (prettyText e))
            where
                diff = ind `M.difference` wit
    toEither $ error_list 
        $ zipWith err evs vs
    return tr

tr_hint' :: HasMachineP2 mch
         => mch
         -> HashMap Name Var
         -> NonEmpty Label
         -> LatexDoc
         -> TrHint
         -> RWS LineInfo [Error] () TrHint
tr_hint' p2 fv lbls = visit_doc []
        [ ( "\\index"
          , CmdBlock $ \(x, texExpr) (TrHint ys z) -> do
                evs <- _unM $ get_events p2 lbls
                let inds = p2^.pIndices
                vs <- _unM $ bind_all evs 
                    ([st|'%s' is not an index of '%s'|] (renderText x) . prettyText) 
                    (\e -> x `M.lookup` (inds ! e))
                let Var _ t = NE.head vs
                    ind = prime $ Var x t
                    x'  = addPrime x
                expr <- _unM $ hoistEither $ parse_expr' 
                    ((p2^.pMchSynt) `with_vars` insert x' ind fv) 
                    texExpr
                return $ TrHint (insert x (t, expr) ys) z)
        , ( "\\lt"
          , CmdBlock $ \(Identity prog) (TrHint ys z) -> do
                let msg = [st|Only one progress property needed for '%s'|]
                _unM $ toEither $ error_list 
                    [ ( not $ MM.isNothing z
                      , msg $ prettyText $ NE.toList lbls )
                    ]
                return $ TrHint ys (Just prog))
        ]
