{-# LANGUAGE TypeFamilies,CPP,TemplateHaskell,OverloadedStrings #-}
module Logic.Names.Internals 
    ( module Logic.Names.Internals
    , NEText(..)
    )
where

    -- Libraries
import Control.DeepSeq
import qualified Control.Invariant as I
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Precondition

import Data.Char
import Data.Data
import Data.Either.Combinators
import Data.Hashable
import Data.List as L
-- import Data.List.Lens as L
import qualified Data.List.Ordered as Ord
#if MIN_VERSION_semigroups(0,18,0)
import Data.List.NonEmpty as NE
#else
import Data.List.NonEmpty as NE hiding (unlines)
#endif
import qualified Data.Map as M
import Data.Serialize
import Data.Semigroup hiding (option)
import           Data.Text      as T
-- import qualified Data.Text.Lens as T
import Data.Tuple
import Data.Word

import GHC.Generics.Instances

import Language.Haskell.TH hiding (Name)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax hiding (Name,lift)
import qualified Language.Haskell.TH.Syntax as TH 

import Test.QuickCheck as QC 
import Test.QuickCheck.Regression as QC
import Test.QuickCheck.Report as QC
import Test.QuickCheck.ZoomEq

import Text.Pretty
import Text.Printf.TH

import Utilities.Language  as Lang

type NEString = NonEmpty Char

#ifdef __LAZY_NAME__

    -- ^ Names can be specified in Z3 or in LaTeX encoding
    -- ^ They can then be rendered in either Z3 or LaTeX
    -- ^ by using `substToZ3` and `substToLatex` to translate them
data Name = Name 
        { _backslash :: Bool 
        , _base :: NEText 
        , _primes :: Word8 
        , _suffix :: Text
        } deriving (Data,Generic,Eq,Ord,Show)

data InternalName = InternalName Text Name Text
    deriving (Eq,Ord,Data,Generic,Show)

#else

    -- ^ Names can be specified in Z3 or in LaTeX encoding
    -- ^ They can then be rendered in either Z3 or LaTeX
    -- ^ by using `substToZ3` and `substToLatex` to translate them
data Name = Name 
        { _backslash :: !Bool 
        , _base :: !NEText 
        , _primes :: !Word8 
        , _suffix :: !Text
        } deriving (Data,Generic,Eq,Ord,Show)

data InternalName = InternalName !Text !Name !Text
    deriving (Eq,Ord,Data,Generic,Show)

#endif

data Encoding = Z3Encoding | LatexEncoding
    deriving (Eq,Ord,Data,Generic,Show)

makeLenses ''Name

name :: Bool -> NEText
             -> Word8
             -> Text
             -> Encoding
             -> Name
name bl base pr suff Z3Encoding = Name bl base pr suff
name bl base pr suff LatexEncoding = Name bl 
        (replaceAll' substToZ3 base) pr 
        (replaceAll substToZ3 suff)

instance PrettyPrintable Name where
    pretty = render

instance PrettyPrintable InternalName where
    pretty = render

class (Show a,Ord a,Hashable a,Data a) => IsBaseName a where
    render :: a -> String
    render = unpack . renderText
    renderText :: a -> Text
    renderText = pack . render
    asInternal' :: a -> InternalName
    asName'  :: a -> Name
    fromString'' :: Pre => String -> a
    fromString'' = fromText . pack
    fromText :: Pre => Text -> a
    fromText = fromString'' . unpack
    addPrime :: a -> a
    generateNames :: a -> [a]
    language :: Proxy a -> Language a
    texName :: Pre 
            => String -> a
    texName = texNameText . pack
    texNameText :: Pre 
                => Text -> a
    texNameText = texName . unpack
    z3Name :: Pre 
           => String -> a
    z3Name = z3NameText . pack
    z3NameText :: Pre 
               => Text -> a
    z3NameText = z3Name . unpack
    {-# MINIMAL (fromText | fromString''), 
                (render | renderText), 
                (texName | texNameText), 
                (z3Name | z3NameText),
                asInternal', asName', addPrime, 
                generateNames, language  #-}

_Name :: IsName a => Prism' Text a
_Name = prism' renderText (fmap fromName . isZ3Name')

renderAsLatex :: Name -> Text
renderAsLatex (Name b (NEText base') p suff') = T.concat [slash,base,T.replicate (fromIntegral p) "\'",suffix]
    where
        base = replaceAll substToLatex base'
        suff = replaceAll substToLatex suff'

        slash  | b          = "\\"
               | otherwise  = ""
        suffix | T.null suff = ""
               | otherwise   = [st|_{%s}|] suff

numbered :: Int -> NEText -> NEText
numbered n (NEText str) = NEText $ str <> pack (show n)

instance IsBaseName Name where
    renderText = renderAsLatex
    asInternal' n = InternalName "" n ""
    asName' = id
    fromText = makeName
    addPrime = primes %~ (+1)
    generateNames n = n : [ n & base %~ numbered i | i <- [0 ..] ]
    language Proxy = latexName
    z3NameText = either (assertFalse' . unpack . T.unlines) id . isZ3Name
    texName = fromString''

class IsBaseName n => IsName n where
    fromInternal :: InternalName -> n
    fromName :: Name -> n

asInternal :: IsName n => n -> InternalName
asInternal = asInternal'

asName :: IsName n => n -> Name    
asName = asName'

instance IsName Name where
    fromInternal = asName
    fromName = id
instance IsName InternalName where
    fromInternal = id
    fromName = asInternal

fresh :: IsBaseName n => n -> M.Map n b -> n
fresh name xs = L.head $ ys `Ord.minus` M.keys xs
    where
        ys = generateNames name

make :: (Pre,IsBaseName n0,IsBaseName n1)
     => (n0 -> n1 -> a)
     -> String -> String -> a
make f inm = make' (make' f inm)

make' :: (Pre,IsBaseName n)
      => (n -> a)
      -> String -> a
make' f = f . fromString''

(<+) :: NonEmpty a -> [a] -> NonEmpty a
(<+) (x :| xs) ys = x :| (xs ++ ys)

instance IsBaseName InternalName where
    renderText (InternalName pre x suf) = prefix <> z3Render x <> suf
        where
            prefix | T.null pre  = ""
                   | otherwise   = [st|@@%s@@_|] pre
    asInternal' = id
    asName' (InternalName _ n _) = n
    fromText = fromText'
    addPrime = internal %~ addPrime
    generateNames (InternalName pre n suf) = 
            InternalName pre <$> generateNames n <*> pure suf
    language Proxy = asInternal' <$> z3Name'
    z3NameText = either (assertFalse' . unpack . T.unlines) id . isZ3InternalName
    texName str = asInternal' $ (texName str :: Name)

instance Hashable Name where
instance Hashable InternalName where
instance Hashable Encoding where

z3Render :: Name -> Text
z3Render (Name sl (NEText xs) ps suf) 
        = T.concat $ [slash,xs,T.replicate (fromIntegral ps) "@prime",suf']
    where
        slash | sl        = "sl$"
              | otherwise = ""
        suf'  | T.null suf  = ""
              | otherwise   = "@" <> suf

setSuffix :: Text -> Name -> Name
setSuffix suff = suffix .~ suff

fromText' :: Pre => Text -> InternalName
fromText' nm = InternalName "" (fromJust' $ isZ3Name' n) suf
    where
        (n,suf) = T.span ('@' /=) nm


isZ3Name' :: Text -> Maybe Name
isZ3Name' = rightToMaybe . isZ3Name

isZ3Name :: Text -> Either [Text] Name
isZ3Name = parseLanguage z3Name'

parseLanguage :: Language a -> Text -> Either [Text] a
parseLanguage lang str = mapLeft (\x -> [err,pack $ show x]) $ parse' lang "" str
    where
        err = [st|invalid name: '%s'|] str

isName :: Text -> Either [Text] Name
isName = parseLanguage latexName

isName' :: Text -> Maybe Name
isName' = either (const Nothing) Just . isName

makeZ3Name :: Pre => Text -> Name
makeZ3Name = fromJust' . isZ3Name'

makeName :: Pre => Text -> Name
makeName = fromJust' . isName'

addBackslash :: Name -> Name
addBackslash = backslash .~ True

addSuffix :: Text -> InternalName -> InternalName
addSuffix n1 (InternalName pre n0 suf) = InternalName pre n0 $ suf <> n1

addPrefix :: Text -> InternalName -> InternalName
addPrefix n1 (InternalName pre n0 suf) = InternalName (n1 <> pre) n0 suf

dropSuffix :: InternalName -> InternalName
dropSuffix (InternalName pre ns _) = InternalName pre ns ""


reserved :: Text -> Int -> InternalName
reserved pre n = InternalName pre (makeName $ pack $ show n) ""

internal :: Lens' InternalName Name
internal f (InternalName pre n suf) = (\n' -> InternalName pre n' suf) <$> f n

isZ3InternalName :: Text -> Either [Text] InternalName
isZ3InternalName = parseLanguage z3InternalName
 
z3InternalName :: Language InternalName
z3InternalName = InternalName <$> (pack <$> prefix) 
                              <*> z3Name' 
                              <*> (option "" $ text "$")
    where
        prefix = option "" (string "@@" >> many1' z3NameChar >> string "@@_")

z3Name' :: Language Name
z3Name' = symb <|> name
    where
        name = 
            Name <$> option False (try (string "sl$" >> pure True)) 
                 <*> many1Text' z3NameChar
                 <*> (fromIntegral.L.length 
                        <$> many (string "@prime"))
                 <*> pure ""
        symb = Name False <$> many1Text' symbol 
                          <*> pure 0 
                          <*> pure ""

z3NameChar :: Language Char
z3NameChar = alphaNum <|> char '-'

latexNameChar :: Language Char
latexNameChar = alphaNum <|> char '_'

latexName :: Language Name
latexName = symb <|> name'
    where
        name' = 
            name <$> option False (string "\\" >> pure True) 
                 <*> many1Text' latexNameChar
                 <*> (fromIntegral.L.length 
                        <$> many (string "\'"))
                 <*> pure ""
                 <*> pure LatexEncoding
        symb = name False <$> (NEText . T.singleton <$> symbol')
                          <*> pure 0 
                          <*> pure ""
                          <*> pure LatexEncoding
        symbol' = symbol <|> texSymbol

texSymbol :: Language Char
texSymbol = oneOf [';','.']

symbol :: Language Char
symbol = (oneOf ['-','*','/'] <|> satisfy isSymbol) <?> "symbol"

data SubstPattern = SPat [(Text,Text)] [(Text,Text)] [(Text,Text)]
    deriving Show

inverse :: SubstPattern -> SubstPattern
inverse (SPat x y z) = SPat (L.map swap x) (L.map swap y) (L.map swap z)

substToZ3 :: SubstPattern
substToZ3 = SPat [("\\","sl$")] [] [("'","@prime")]

substToLatex :: SubstPattern
substToLatex = inverse substToZ3

shuffle' :: SubstPattern -> Gen SubstPattern
shuffle' (SPat x y z) = SPat <$> shuffle x <*> shuffle y <*> shuffle z

replaceAll' :: Pre 
            => SubstPattern -> NEText -> NEText
replaceAll' sub = NEText . replaceAll sub . getText

prefixedText :: Text -> Prism' Text Text
prefixedText pre = prism' 
        (pre <>) 
        (\x -> guard (pre `T.isPrefixOf` x) >> return (T.drop (T.length x) x))

suffixedText :: Text -> Prism' Text Text
suffixedText suf = prism' 
        (<> suf) 
        (\x -> guard (suf `T.isSuffixOf` x) >> return (T.drop (T.length x) x))

preSubtituted :: Text -> (Text,Text) -> Maybe (Text,Text)
preSubtituted xs (pat,sub) = (sub,) <$> xs^?prefixedText pat

postSubtituted :: Text -> (Text,Text) -> Maybe (Text,Text)
postSubtituted xs (pat,sub) = (,sub) <$> xs^?suffixedText pat

midSubtituted :: Text -> (Text,Text) -> Maybe (Text,Text)
midSubtituted xs (pat,sub) = (_1 %~ (<> sub)) <$> xs^?foldSplits . below (prefixedText pat)

foldSplits :: Fold Text (Text,Text)
foldSplits = folding $ \xs -> L.zip (T.inits xs) (T.tails xs)

replaceAll :: SubstPattern -> Text -> Text
replaceAll (SPat pre mid suff) = substPre
    where
        substPre xs = fromMaybe (substPost xs) $ do
                (p,s) <- pre^?traverse.folding (preSubtituted xs)
                return $ p <> substPre s
        substPost xs = fromMaybe (substMid xs) $ do
                (p,s) <- suff^?traverse.folding (postSubtituted xs)
                return $ substPost p <> s
        substMid xs = fromMaybe xs $ do
                (p,s) <- mid^?traverse.folding (midSubtituted xs)
                return $ p <> substMid s

smt :: QuasiQuoter
smt = QuasiQuoter
    { quoteExp  = \str -> [e| fromName $ $(parseZ3Name str) |]
    , quotePat  = undefined
    , quoteDec  = undefined
    , quoteType = undefined }

tex :: QuasiQuoter
tex = QuasiQuoter
    { quoteExp  = \str -> [e| $(parseTexName str) |]
    , quotePat  = undefined
    , quoteDec  = undefined
    , quoteType = undefined }

parseZ3Name :: String -> ExpQ
parseZ3Name str = either (fail . unpack . T.unlines) TH.lift $ isZ3Name $ pack str

parseTexName :: String -> ExpQ
parseTexName str = either (fail . unpack . T.unlines) TH.lift $ isName $ pack str

prop_subst_idempotent :: Text -> Property
prop_subst_idempotent xs = replaceAll substToZ3 (replaceAll substToZ3 xs) === replaceAll substToZ3 xs

prop_rev_substToZ3_idempotent :: Text -> Property
prop_rev_substToZ3_idempotent xs = replaceAll substToLatex (replaceAll substToLatex xs) === replaceAll substToLatex xs

prop_subst_order_independent :: Text -> Property
prop_subst_order_independent xs = forAll (shuffle' substToZ3) $ \s -> replaceAll s xs === replaceAll substToZ3 xs

prop_rev_subst_order_independent :: Text -> Property
prop_rev_subst_order_independent xs = forAll (shuffle' substToLatex) $ \s -> replaceAll s xs === replaceAll substToLatex xs

prop_subst_left_inv :: Name -> Property
prop_subst_left_inv xs = 
        replaceAll substToLatex (replaceAll substToZ3 $ renderText xs) === renderText xs

prop_subst_left_inv_regression :: Property
prop_subst_left_inv_regression = regression
        prop_subst_left_inv
        [ name0, name1, name2 ]

name0 :: Name
name0 = Name True (NEText "sl") 1 ""

name1 :: Name
name1 = Name True (NEText "prime") 1 ""

name2 :: Name
name2 = Name {_backslash = False, _base = NEText "sl", _primes = 1, _suffix = ""}

prop_subst_right_inv :: InternalName -> Property
prop_subst_right_inv xs = 
        replaceAll substToZ3 (replaceAll substToLatex $ renderText xs) === renderText xs

prop_subst_preserves_non_emptiness :: NEText -> Property
prop_subst_preserves_non_emptiness (NEText xs) = replaceAll substToZ3 xs =/= ""

prop_substToLatex_preserves_non_emptiness :: NEText -> Property
prop_substToLatex_preserves_non_emptiness (NEText xs) = replaceAll substToLatex xs =/= ""

prop_insertAt :: Int -> Text -> NEText -> Property
prop_insertAt n xs ys = getText (insertAt n xs ys) === T.take n ys' <> xs <> T.drop n ys'
    where
        ys' = getText ys

prop_render_isomorphic :: Name -> Name -> Property
prop_render_isomorphic xs ys = counterexample 
        (show (render xs) ++ "\n" ++ show (render ys))
        $ (render xs == render ys) === (xs == ys)

nonEmptyOf :: Gen a -> Gen (NonEmpty a)
nonEmptyOf gen = (:|) <$> gen <*> listOf gen

infix 4 =/=
(=/=) :: (Eq a, Show a) => a -> a -> Property
x =/= y = counterexample (show x ++ " == " ++ show y) (x /= y)

insertAt :: Int -> Text -> NEText -> NEText
insertAt n xs (NEText ys) = 
    NEText (T.take n ys <> xs <> T.drop n ys)

instance ZoomEq Name where
    (.==) = (I.===)

instance ZoomEq InternalName where
    (.==) = (I.===)

instance Arbitrary Name where
    arbitrary = do
        r <- oneof 
            [ word latexName
            , word z3Name' ]
        let sl    = NEText "sl"
            prime = NEText "prime"
        oneof 
            [ do 
                n <- choose (0,3)
                let cmd n = do
                        i  <- QC.elements [0,T.length $ getText $ n^.base]
                        kw <- QC.elements ["sl","prime"]
                        return $ ((),n & base %~ insertAt i kw)
                execStateT (replicateM_ n $ StateT cmd) r
            , r & base (const $ sconcat <$> nonEmptyOf (QC.elements [sl,prime])) ]
    shrink = genericShrink

instance Arbitrary InternalName where
    arbitrary = do
        asInternal' <$> (arbitrary :: Gen Name)
    shrink = genericShrink

instance TH.Lift Name where
    lift (Name a b c d) = [e| name a b c d Z3Encoding |]
instance Lift Encoding where
    lift = genericLift
instance Lift InternalName where
    lift = genericLift

instance NFData Encoding where
instance NFData Name where
instance NFData InternalName where
instance Serialize Encoding where
instance Serialize Name where
instance Serialize InternalName where

class Translatable a b | a -> b where
    translate :: a -> b

return []

run_props :: (PropName -> Property -> IO (a, QC.Result))
          -> IO ([a], Bool)
run_props = $forAllProperties'
