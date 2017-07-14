{-# LANGUAGE TypeFamilies, CPP #-}
module Logic.Names 
    ( IsBaseName(..) 
    , HasNames(..) 
    , NEText(..)
    , nameType
    , Name(..), InternalName(..)
    , isZ3Name, isZ3Name'
    , Translatable(..)
    , IsName(..)
    , asInternal, asName
    , makeName
    , makeZ3Name
    , make, make'
    , isName, isName'
    -- , fromString'
    , fresh
    , reserved
    , z3Render
    , dropSuffix 
    , addSuffix
    , addPrefix
    , addBackslash
    , setSuffix
    , smt, tex
    , pack
    , Encoding(..)
    , run_props     
    )
where

    -- Modules
#ifdef __PACKAGED_NAMES__
import Logic.Names.Packaged as Names
#else
import Logic.Names.Internals as Names
#endif

import Control.Lens
import Data.Text (pack)
import Data.Typeable
import Data.HashMap.Lazy.Extras (Key)

class HasNames a n | a -> n where
    type SetNameT n' a :: *
    namesOf :: Key n' 
            => Traversal a (SetNameT n' a)
                         n n'

nameType :: String
nameType = tyConModule $ fst $ splitTyConApp $ typeRep (Proxy :: Proxy Name)
