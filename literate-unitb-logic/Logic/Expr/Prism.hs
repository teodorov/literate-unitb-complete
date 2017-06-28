{-# LANGUAGE TemplateHaskell #-}
module Logic.Expr.Prism 
    ( module Logic.Names 
    , module Logic.Expr.Prism )
where

    -- Modules
import Logic.Expr.Classes
import Logic.Expr.Expr
import Logic.Names

    -- Libraries
import Control.Lens hiding (uncons)

import Data.List.Lens
import Data.Maybe
import Data.String.Utils
import           Data.Text (unpack)
import qualified Data.Text as T

import Language.Haskell.TH hiding (Name)
import Language.Haskell.TH.Quote

fun :: QuasiQuoter
fun = QuasiQuoter 
        { quoteExp  = funPrism . makePattern
        , quoteType = undefined
        , quotePat  = funPat
        , quoteDec  = undefined }

{-# INLINE selectFun #-}
selectFun :: Eq n => n -> Traversal' (GenExpr n t a q) [GenExpr n t a q]
selectFun x = _FunApp . selectFun' x

selectFun' :: Eq n => n -> Traversal' (AbsFun n a,[GenExpr n t a q]) [GenExpr n t a q]
selectFun' fn f (fn',args') | fn == (fn'^.name) = (fn',) <$> f args'
                            | otherwise         = pure (fn',args')

matchLength :: Int 
            -> ( [GenExpr n t a q] -> k )
            -> Fold [GenExpr n t a q] k
matchLength recSize f = filtered ((recSize ==) . length) . to f

zipRecord' :: [Maybe String] -> ExpQ
zipRecord' args = 
        [e| matchLength ($recSize) (\_args -> $(myLet)) |]
    where
        recSize = litE $ integerL $ fromIntegral $ length fieldPos
        decs = map (binding . snd) fieldPos
        decs :: [DecQ]
        binding n = valD (varP $ mkName $ "x" ++ show n) 
                         (normalB [e| $(varE $ mkName "_args") !! $(litE $ integerL $ fromIntegral n) |]) []
        myLet = letE decs $ tupE [ varE (mkName $ "x" ++ show i) | (_,i) <- fieldPos ]
        fieldPos = mapMaybe (sequenceOf _1) $ zip args [0 :: Int ..]

funPrism :: Pattern -> ExpQ 
funPrism (Pattern f args) = [e| selectFun (fromName f) . $(zipRecord' args) |]

fieldTuple :: [String] -> PatQ
fieldTuple kw = tupP $ map (varP . mkName) kw

tuplePrism :: Pattern -> PatQ
tuplePrism (Pattern _ args) = [p| Just $(fieldTuple $ catMaybes args) |]

data Pattern = Pattern Name [Maybe String]

makePattern :: String -> Pattern
makePattern str = Pattern kw' args''
    where
        inside = fromMaybe (error "expecting parenthesis around S-expression")
                    $ strip str^?prefixed "(".suffixed ")"
        (kw,args) = fromMaybe (error "expecting at least one keyword")
                    $ inside^?partsOf worded._Cons
        args' = fromMaybe (error "field names should start with '$'")
                    $ args^?below (prefixed "$")
        kw' :: Name
        kw' = either (error . unpack . T.unlines) id $ isZ3Name $ pack kw
        args'' = map (^? filtered (/= "_")) args'

funPat :: String -> PatQ
funPat str = viewP 
        [e| preview $(funPrism pat) |] 
        (tuplePrism pat)
    where
        pat = makePattern str
