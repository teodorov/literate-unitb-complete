{-# LANGUAGE ScopedTypeVariables #-}
module Document.Phase.Parameters where

    -- Modules
import Latex.Parser

import Logic.Expr

import UnitB.Syntax

    -- Libraries
import Control.Applicative
import Control.Category
import Control.Lens
import Control.Monad.State

import Data.Either.Combinators
import Data.Either.Validation
import Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Monoid
import Data.Proxy.TH
import           Data.Text (Text,unpack)
import qualified Data.Text as T
import Prelude hiding ((.),id)

import Text.Printf.TH
import Text.Read

import Utilities.Syntactic

newtype TheoryName = TheoryName { getTheoryName :: Name }

newtype RuleName = RuleName Text

newtype VarName = VarName { getVarName :: Name }

newtype IndName = IndName { getIndName :: Name }

newtype SetName = SetName { getSetName :: Name }

newtype ExprText = Expr { getExprText :: StringLi }

newtype PlainText = PlainText { getPlainText :: LatexDoc }

newtype PO = PO { getPO :: Text }

newtype CoarseSchLbl = CoarseSchLbl { getCoarseSchLbl :: Label }

newtype FineSchLbl = FineSchLbl { getFineSchLbl :: Label }

newtype GuardLbl = GuardLbl { getGuardLbl :: Label }

newtype ActionLbl = ActionLbl { getActionLbl :: Label }

newtype InitLbl = InitLbl { getInitLbl :: Label }

newtype NewLabel = NewLabel { getNewLabel :: Label }

newtype EventOrRef = EventOrRef { getEventOrRef :: Label }

newtype Factor = Factor { getFactor :: Float }

--type M = Either [Error]

readText :: LatexDoc -> Either [Error] Text
readText x = do
        let x' = trim_blank_text' x
        case isWord x' of
            Just x
                -> Right x
            _   -> err_msg x'
    where
        err_msg x = Left [Error "expecting a word" $ line_info x]

readName :: LineInfo -> Text -> Either [Error] Name
readName li str' = do
    let str = T.strip str'
    with_li li $ isName str

read_ :: Read a
      => LineInfo 
      -> Text 
      -> Either [Error] a
read_ li str' = do
    let str = T.strip str'
    with_li li $ mapLeft (pure.[st|Invalid value: %s|]) $ readEither $ unpack str

readFromText :: (Text -> a) -> LineInfo -> Text -> Either [Error] a
readFromText f _ = return . f

class LatexArgFromText a where
    kind' :: Proxy a -> Text
    read_one' :: LineInfo -> Text -> Either [Error] a

class LatexArg a where
    argKind :: Proxy a -> Text
    default argKind :: LatexArgFromText a => Proxy a -> Text
    argKind = kind'
    read_one :: LatexDoc -> Either [Error] a
    default read_one :: LatexArgFromText a => LatexDoc -> Either [Error] a
    read_one doc = read_one' (line_info doc) =<< readText doc


instance LatexArg Factor where
instance LatexArgFromText Factor where
    kind' Proxy = "factor"
    read_one' li = fmap Factor . read_ li

instance LatexArg TheoryName where
instance LatexArgFromText TheoryName where
    kind' Proxy = "theory-name"
    read_one' li = fmap TheoryName . readName li

instance LatexArg MachineId where
instance LatexArgFromText MachineId where
    kind' Proxy = "machine-name"
    read_one' li = fmap MId . readName li

instance LatexArg RuleName where
instance LatexArgFromText RuleName where
    kind' Proxy = "rule"
    read_one' = readFromText RuleName

instance LatexArg ProgId where
instance LatexArgFromText ProgId where
    kind' Proxy = "progress-property-label"
    read_one' = readFromText $ PId . label

instance LatexArg VarName where
instance LatexArgFromText VarName where
    kind' Proxy = "variable"
    read_one' li = fmap VarName . readName li

instance LatexArg IndName where
instance LatexArgFromText IndName where
    kind' Proxy = "index"
    read_one' li = fmap IndName . readName li

instance LatexArg SetName where
instance LatexArgFromText SetName where
    kind' Proxy = "set-name"
    read_one' li = fmap SetName . readName li

instance LatexArgFromText EventId where
    kind' Proxy = "event-name"
    read_one' = readFromText $ EventId . label

instance LatexArg ExprText where
    argKind Proxy = "expression"
    read_one = return . Expr . flatten_li' . trim_blank_text'

instance LatexArgFromText CoarseSchLbl where
    kind' Proxy = "coarse-schedule-label"
    read_one' = readFromText $ CoarseSchLbl . label

instance LatexArgFromText FineSchLbl where
    kind' Proxy = "fine-schedule-label"
    read_one' = readFromText $ FineSchLbl . label

instance LatexArgFromText ActionLbl where
    kind' Proxy = "action-label"
    read_one' = readFromText $ ActionLbl . label

instance LatexArgFromText GuardLbl where
    kind' Proxy = "guard-label"
    read_one' = readFromText $ GuardLbl . label

instance LatexArgFromText InitLbl where
    kind' Proxy = "initialization-label"
    read_one' = readFromText $ InitLbl . label

instance LatexArg PO where
    read_one = return . PO . flatten
instance LatexArgFromText PO where
    kind' Proxy = "proof-obligation-name"
    read_one' = readFromText $ PO

instance LatexArg NewLabel where
instance LatexArgFromText NewLabel where
    kind' Proxy = "fresh-name"
    read_one' = readFromText $ NewLabel . label

instance LatexArg EventOrRef where
instance LatexArgFromText EventOrRef where
    kind' Proxy = "event-or-progress-property"
    read_one' = readFromText $ EventOrRef . label

instance LatexArg PlainText where
    argKind Proxy = "text / comment"
    read_one = return . PlainText

instance LatexArgFromText a => LatexArgFromText (Conc a) where
    read_one' li = fmap Conc . read_one' li
    kind' p = "concrete " <> kind' (getConcrete <$> p)
instance LatexArgFromText a => LatexArgFromText (Abs a) where
    read_one' li = fmap Abs . read_one' li
    kind' p = "abstract " <> kind' (getAbstract <$> p)
instance LatexArgFromText a => LatexArgFromText (Common a) where
    read_one' li = fmap Common . read_one' li
    kind' p = "preserved " <> kind' (getCommon <$> p)
instance LatexArgFromText a => LatexArg (Conc a) where
    argKind p = "concrete " <> kind' (getConcrete <$> p)
instance LatexArgFromText a => LatexArg (Abs a) where
    argKind p = "abstract " <> kind' (getAbstract <$> p)
instance LatexArgFromText a => LatexArg (Common a) where
    argKind p = "preserved " <> kind' (getCommon <$> p)
instance LatexArgFromText a => LatexArg [a] where
    argKind p = "zero or more " <> kind' (head <$> p)
    read_one doc = validationToEither $ parseAll $ comma_sep $ flatten doc
        where 
            parseAll = traverse (eitherToValidation . read_one' (line_info doc))
instance LatexArgFromText a => LatexArg (NonEmpty a) where
    argKind p = "one or more " <> kind' (NE.head <$> p)
    read_one doc = toEither $ fmap parseAll $ NE.nonEmpty $ comma_sep $ flatten doc
        where 
            msg = "expecting at one or more " <> kind' [pr|a|] <> " argument"
            toEither = validationToEither . fromMaybe (Failure [Error msg $ line_info doc])
            parseAll = traverse (eitherToValidation . read_one' (line_info doc))

newtype Abs a = Abs { getAbstract :: a }
    deriving (Eq,Ord)

newtype Conc a = Conc { getConcrete :: a }
    deriving (Eq,Ord)

newtype Common a = Common { getCommon :: a }
    deriving (Eq,Ord)
