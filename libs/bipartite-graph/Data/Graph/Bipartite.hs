{-# LANGUAGE CPP,QuasiQuotes #-}
module Data.Graph.Bipartite
    ( BiGraph,GraphBuilder,GraphReader,BiGraph'
    , source, target, origins
    , leftKey,leftInfo,rightInfo,rightKey
    , predecessors, successors
    , getLeftVertices, getRightVertices
    , getLeftVertex, getRightVertex
    , leftVertex, rightVertex
    , leftVertices, rightVertices
    , getEdges
    , fromList, fromList', empty, makeGraph
    , newEdge'
    , newEdge, newLeftVertex, newRightVertex
    , mapLeft, mapRight, mapBoth
    , Data.Graph.Bipartite.mapKeys
    , Data.Graph.Bipartite.keys
    , insertEdge
    , mapLeftWithKey
    , mapRightWithKey
    , mapBothWithKey
    , mapEdges 
    , traverseLeft, traverseRight, traverseBoth
    , traverseLeftWithKey, traverseRightWithKey
    , traverseLeftWithEdges, traverseRightWithEdges
    , traverseLeftWithEdges', traverseRightWithEdges'
    , traverseLeftWithEdgeInfo', traverseRightWithEdgeInfo'
    , traverseLeftWithEdgeInfo, traverseRightWithEdgeInfo
    , traverseEdges, traverseEdgesWithKeys
#if MIN_VERSION_transformers(0,5,0)
    , liftEqGraph
    , liftEqMap
    , liftShowsGraph
    , liftShowsListPrec
    , liftShowsMap
#endif
    , acrossBoth
    , transpose
    , leftMap, rightMap, edgeMap
    , readGraph, forwardEdges, backwardEdges
    , hasLeftVertex, hasRightVertex
    , hasEdge, edgeInfo 
    , lookup, leftLookup, rightLookup
    , member, leftMember, rightMember 
    , acrossBothWithKey )
where

import Control.Applicative hiding (empty)
import Control.Arrow
import Control.DeepSeq
import Control.Lens

import Control.Monad.Reader
import Control.Monad.RWS hiding ((<>))
import Control.Precondition

import Data.Array as A hiding ((!))
import Data.Array.ST
import Data.Default
#if MIN_VERSION_transformers(0,5,0)
import Data.Functor.Classes
#endif
import Data.List  as L hiding (transpose,lookup)
import Data.List.NonEmpty  as NE hiding (fromList,transpose)
import Data.HashMap.Lazy   as M hiding (fromList,empty,traverseWithKey,lookup,member,(!))
import qualified Data.HashMap.Lazy   as M hiding ((!))
import Data.Semigroup
import Data.Serialize
import Data.Serialize.Instances ()
import qualified Data.Traversable as T
import Data.Tuple

import GHC.Generics hiding (from,to)

import Prelude hiding (lookup)

import Test.QuickCheck.ZoomEq

import Text.Printf.TH

newtype GraphBuilder key0 v0 key1 v1 e s0 s1 a = GB (RWST () ([(key0,v0)],[(key1,v1)],[(Int,Int,e)]) (Int,HashMap key0 Int,Int,HashMap key1 Int) Maybe a)
    deriving (Monad,Applicative,Functor,Alternative,MonadPlus)

type GraphReader key v0 v1 s0 s1 a = GraphReader' key v0 key v1 () s0 s1 a

type GraphReader' key0 v0 key1 v1 e s0 s1 = GraphReaderT key0 v0 key1 v1 e s0 s1 Identity

newtype GraphReaderT key0 v0 key1 v1 e s0 s1 m a = GR (ReaderT (BiGraph' key0 v0 key1 v1 e) m a)
    deriving (Monad,Applicative,Functor)

type BiGraph key v0 v1 = BiGraph' key v0 key v1 ()

data BiGraph' key0 v0 key1 v1 e = Graph 
                { _leftAL  :: AdjList key0 v0
                , _rightAL :: AdjList key1 v1
                , _edges :: HashMap (Int,Int) e }
    deriving (Generic)

data AdjList key v0 = AList 
                { _arKey :: Array Int key
                , _arVals  :: Array Int v0 
                , _arEdges :: Array Int (NonEmpty Int)
                , _mapKey  :: HashMap key Int }
    deriving (Generic)

newtype Vertex s = Vertex Int
    deriving (Ord,Eq,Generic)

data Edge s0 s1 = Edge Int Int
    deriving (Ord,Eq,Generic)

makeLenses ''BiGraph'
makeLenses ''AdjList
instance NFData (Edge s0 s1)
instance NFData (Vertex s0)

instance (Ord key0, Ord key1, Eq v0, Eq v1, Eq e) => Eq (BiGraph' key0 v0 key1 v1 e) where
    g0 == g1 = f g0 == f g1
        where
            f x = (edgeMap x, leftMap x, rightMap x)

fromList :: (Ord key0,Ord key1)
         => [(key0,v0)] 
         -> [(key1,v1)] -> [(key0,key1)] 
         -> Maybe (BiGraph' key0 v0 key1 v1 ())
fromList v0 v1 es = makeGraph $ do
    mapM_ (uncurry newLeftVertex) v0
    mapM_ (uncurry newRightVertex) v1
    mapM_ (uncurry addEdge) es

fromList' :: (Ord key0,Ord key1,Pre)
          => [(key0,v0)] 
          -> [(key1,v1)] -> [(key0,key1)] 
          -> BiGraph' key0 v0 key1 v1 ()
fromList' xs ys zs = fromJust' $ fromList xs ys zs

empty :: BiGraph' key0 v0 key1 v1 e
empty = Graph emptyList emptyList M.empty
    where

emptyList :: AdjList key vs
emptyList = AList emptyAr emptyAr emptyAr M.empty

mapALKey :: Ord k1 => (k0 -> k1) -> AdjList k0 v -> AdjList k1 v
mapALKey f al = al { _arKey = al^.arKey & traverse %~ f, _mapKey = M.mapKeys f $ al^.mapKey }

emptyAr :: Array Int e
emptyAr = listArray (0,-1) []

keys :: Lens (AdjList k0 v) (AdjList k1 v) (Array Int k0,HashMap k0 Int) (Array Int k1,HashMap k1 Int)
keys f x = (\(y,z) -> AList y (x^.arVals) (x^.arEdges) z) <$> y
    where
        y = f (x^.arKey,x^.mapKey)

edgeList :: BiGraph' k0 v0 k1 v1 e -> [(k0,v0,k1,v1,e)]
edgeList g = M.elems $ M.mapWithKey (\(i,j) e -> 
        ((lv^.arKey) ! i,(lv^.arVals) ! i,(rv^.arKey) ! j,(rv^.arVals) ! j,e)) es
    where
        es = g^.edges
        lv = g^.leftAL
        rv = g^.rightAL

instance Default (BiGraph' key0 v0 key1 v1 e) where
    def = empty

makeGraph :: (forall s0 s1. GraphBuilder key0 v0 key1 v1 e s0 s1 ())
          -> Maybe (BiGraph' key0 v0 key1 v1 e)
makeGraph (GB g) = do
        (table,(vs0,vs1,es)) <- execRWST g () (0,M.empty,0,M.empty)
        let n   = L.length vs0
            m   = L.length vs1
            esM = M.fromList $ L.map (\x -> ((x^._1,x^._2),x^._3)) es
            k0  = listArray (0,n-1) $ L.map fst vs0
            v0  = listArray (0,n-1) $ L.map snd vs0
            k1  = listArray (0,m-1) $ L.map fst vs1
            v1  = listArray (0,m-1) $ L.map snd vs1
        lnAr0 <- traverse nonEmpty $ runSTArray $ do
            ar <- newArray (0,n-1) []
            forM_ es $ \(i,j,_) -> do
                writeArray ar i =<< (j:) <$> readArray ar i
            return ar
        lnAr1 <- traverse nonEmpty $ runSTArray $ do
            ar <- newArray (0,m-1) []
            forM_ es $ \(i,j,_) -> do
                writeArray ar j =<< (i:) <$> readArray ar j
            return ar
        let leftA  = AList k0 v0 lnAr0 (table^._2)
            rightA = AList k1 v1 lnAr1 (table^._4)
        return $ Graph leftA rightA esM -- (listArray (0,n-1) vs) lnAr eAr (snd m)
    where

leftVertices :: Traversal (BiGraph' key0 vA key1 v e) 
                          (BiGraph' key0 vB key1 v e)
                          (key0,vA)
                          vB
leftVertices = leftAL.arKeyVal

rightVertices :: Traversal (BiGraph' key0 v key1 vA e) 
                           (BiGraph' key0 v key1 vB e)
                           (key1,vA)
                           vB
rightVertices = rightAL.arKeyVal

arKeyVal :: Traversal (AdjList keyA vA) (AdjList keyA vB) (keyA,vA) vB
arKeyVal f g = arVals (zipArrayWithM f' (g^.arKey)) g
    where
        f' = curry f

zipArrayWithM :: Applicative f
              => (a -> b -> f c)
              -> Array Int a
              -> Array Int b
              -> f (Array Int c)
zipArrayWithM f a0 a1 = array (i,j) <$> traverse (\i -> (i,) <$> f (a0 ! i) (a1 ! i)) [i..j]
    where
        i = fst (bounds a0) `max` fst (bounds a0)
        j = snd (bounds a1) `min` snd (bounds a1)

newLeftVertex :: Ord key0 => key0 -> v0 -> GraphBuilder key0 v0 key1 v1 e s0 s1 (Vertex s0)
newLeftVertex k v = GB $ do
    c <- use $ _2 . to (M.lookup k)
    case c of
      Nothing -> do
        n <- use _1
        _1 .= (n+1)
        _2 %= M.insert k n
        tell ([(k,v)],[],[])
        return $ Vertex n
      Just n -> return $ Vertex n

getLeftVertex :: Ord key0 => key0 -> GraphBuilder key0 v0 key1 v1 e s0 s1 (Vertex s0)
getLeftVertex k = do
    m <- GB $ use _2
    case k `M.lookup` m of
        Just i -> return (Vertex i)
        Nothing -> mzero

newRightVertex :: Ord key1 => key1 -> v1 -> GraphBuilder key0 v0 key1 v1 e s0 s1 (Vertex s1)
newRightVertex k v = GB $ do
    c <- use $ _4 . to (M.lookup k)
    case c of
      Nothing -> do
        n <- use _3
        _3 .= (n+1)
        _4 %= M.insert k n
        tell ([],[(k,v)],[])
        return $ Vertex n
      Just n -> return $ Vertex n

getRightVertex :: Ord key1 => key1 -> GraphBuilder key0 v0 key1 v1 e s0 s1 (Vertex s1)
getRightVertex k  = do
    m <- GB $ use _4
    case k `M.lookup` m of
        Just i -> return (Vertex i)
        Nothing -> mzero

newEdge :: Vertex s0 -> Vertex s1 -> GraphBuilder key0 v0 key1 v1 () s0 s1 ()
newEdge u v = newEdge' u v ()

newEdge' :: Vertex s0 -> Vertex s1 -> e -> GraphBuilder key0 v0 key1 v1 e s0 s1 ()
newEdge' (Vertex u) (Vertex v) e = GB $ do
    tell ([],[],[(u,v,e)])

addEdge :: (Ord key0,Ord key1) => key0 -> key1 -> GraphBuilder key0 v0 key1 v1 () s0 s1 ()
addEdge k0 k1 = do
    v0 <- getLeftVertex k0
    v1 <- getRightVertex k1
    newEdge v0 v1

mapLeft :: (vA -> vB)
        -> BiGraph' key0 vA key1 v1 e
        -> BiGraph' key0 vB key1 v1 e
mapLeft f = traverseLeft %~ f

mapRight :: (vA -> vB)
         -> BiGraph' key0 v0 key1 vA e
         -> BiGraph' key0 v0 key1 vB e
mapRight f = traverseRight %~ f

mapLeftWithKey :: (key -> vA -> vB)
               -> BiGraph key vA v1 
               -> BiGraph key vB v1
mapLeftWithKey f g = g & leftAL.arVals %~ mapF
    where
        mapF ar = array (bounds ar) $ L.map (uncurry applyF) $ A.assocs ar
        applyF i e = (i,f ((g^.leftAL.arKey) ! i) e)

mapRightWithKey :: (key -> vA -> vB)
                -> BiGraph key v0 vA 
                -> BiGraph key v0 vB
mapRightWithKey f g = g & rightAL.arVals %~ mapF
    where
        mapF ar = array (bounds ar) $ L.map (uncurry applyF) $ A.assocs ar
        applyF i e = (i,f ((g^.rightAL.arKey) ! i) e)
        -- f' = g^.rightAL.arKey & traverse %~ f

traverseLeft :: Traversal (BiGraph' key0 vA key1 v1 e) (BiGraph' key0 vB key1 v1 e) vA vB
traverseLeft = leftAL.arVals.traverse

traverseLeftWithKey :: Traversal (BiGraph' key0 vA key1 v1 e) (BiGraph' key0 vB key1 v1 e) (key0,vA) vB
traverseLeftWithKey = leftVertices

traverseRightWithKey :: Traversal (BiGraph' key0 v1 key1 vA e) (BiGraph' key0 v1 key1 vB e) (key1,vA) vB
traverseRightWithKey = rightVertices

traverseRight :: Traversal (BiGraph' key0 v0 key1 vA e) (BiGraph' key0 v0 key1 vB e) vA vB
traverseRight = rightAL.arVals.traverse

traverseBoth :: Traversal (BiGraph' key0 vA key1 vA e) (BiGraph' key0 vB key1 vB e) vA vB
traverseBoth f (Graph lf rt ed) = Graph <$> (arVals.traverse) f lf 
                                        <*> (arVals.traverse) f rt 
                                        <*> pure ed

traverseRightWithEdgeInfo :: IndexedTraversal k1 (BiGraph' k0 v0 k1 vA e) (BiGraph' k0 v0 k1 vB e) (vA,NonEmpty (k0,v0)) vB
traverseRightWithEdgeInfo f = traverseRightWithEdgeInfo' $ Indexed $ \k -> indexed f k.second (fmap fst)

traverseRightWithEdgeInfo' :: IndexedTraversal k1 (BiGraph' k0 v0 k1 vA e) (BiGraph' k0 v0 k1 vB e) (vA,NonEmpty ((k0,v0),e)) vB
traverseRightWithEdgeInfo' f gr = gr'
    where
        alist = gr^.rightAL
        alist' = gr^.leftAL
        gr' = gr & (rightAL.arVals) (itraverse (\i -> indexed f ((alist^.arKey) ! i).vert i))
        vert i x = (x,incoming i <$> (alist^.arEdges) ! i)
        incoming i j = (((alist'^.arKey) ! j, (alist'^.arVals) ! j), (gr^.edges) ! (j,i))

traverseRightWithEdges' :: IndexedTraversal k1 (BiGraph' k0 v0 k1 vA e) (BiGraph' k0 v0 k1 vB e) (vA,NonEmpty (k0,e)) vB
traverseRightWithEdges' f = traverseRightWithEdgeInfo' $ Indexed $ \k -> indexed f k.second (fmap $ first fst)

traverseRightWithEdges :: IndexedTraversal k1 (BiGraph' k0 v0 k1 vA e) (BiGraph' k0 v0 k1 vB e) (vA,NonEmpty k0) vB
traverseRightWithEdges = traverseRightWithEdges'.trav
    where
        trav f = Indexed $ \k (x,xs) -> indexed f k (x,fst <$> xs)

traverseLeftWithEdgeInfo :: IndexedTraversal k0 (BiGraph' k0 vA k1 v1 e) (BiGraph' k0 vB k1 v1 e) (vA,NonEmpty (k1,v1)) vB
traverseLeftWithEdgeInfo f = traverseLeftWithEdgeInfo' $ lmap (second (fmap fst)) f

traverseLeftWithEdgeInfo' :: IndexedTraversal k0 (BiGraph' k0 vA k1 v1 e) (BiGraph' k0 vB k1 v1 e) (vA,NonEmpty ((k1,v1),e)) vB
traverseLeftWithEdgeInfo' f gr = gr'
    where
        alist = gr^.leftAL
        alist' = gr^.rightAL
        gr' = gr & (leftAL.arVals) (itraverse (\i -> indexed f ((alist^.arKey) ! i).vert i))
        vert i x = (x,incoming i <$> (alist^.arEdges) ! i)
        incoming i j = (((alist'^.arKey) ! j, (alist'^.arVals) ! j), (gr^.edges) ! (i,j))

traverseLeftWithEdges' :: IndexedTraversal k0 (BiGraph' k0 vA k1 v0 e) (BiGraph' k0 vB k1 v0 e) (vA,NonEmpty (k1,e)) vB
traverseLeftWithEdges' f = traverseLeftWithEdgeInfo' $ lmap (second (fmap $ first fst)) f

traverseLeftWithEdges :: IndexedTraversal k0 (BiGraph' k0 vA k1 v0 e) (BiGraph' k0 vB k1 v0 e) (vA,NonEmpty k1) vB
traverseLeftWithEdges = traverseLeftWithEdges'.trav
    where
        trav f = Indexed $ \k (x,xs) -> indexed f k (x,fst <$> xs)

traverseEdges :: Traversal (BiGraph' k0 v0 k1 v1 eA) (BiGraph' k0 v0 k1 v1 eB) (v0,v1,eA) eB
traverseEdges = traverseEdgesWithKeys . (.f)
    where
        f (_,x,_,y,e) = (x,y,e)

traverseEdgesWithKeys :: Traversal (BiGraph' k0 v0 k1 v1 eA) (BiGraph' k0 v0 k1 v1 eB) (k0,v0,k1,v1,eA) eB
traverseEdgesWithKeys f gr = gr & edges (M.traverseWithKey g)
    where
        g (i,j) e = f (k0,v0,k1,v1,e)
            where
                k0 = (gr^.leftAL.arKey ) ! i
                v0 = (gr^.leftAL.arVals) ! i 
                k1 = (gr^.rightAL.arKey ) ! j
                v1 = (gr^.rightAL.arVals) ! j 

acrossBothWithKey :: Applicative f 
                  => (key0 -> vA0 -> f vB0)
                  -> (key1 -> vA1 -> f vB1)
                  -> (e0 -> f e1)
                  -> BiGraph' key0 vA0 key1 vA1 e0
                  -> f (BiGraph' key0 vB0 key1 vB1 e1)
acrossBothWithKey f g h (Graph lf rt ed) = 
            Graph <$> arKeyVal (uncurry f) lf 
                  <*> arKeyVal (uncurry g) rt
                  <*> traverse h ed

acrossBoth :: Applicative f 
           => (vA0 -> f vB0)
           -> (vA1 -> f vB1)
           -> (e0 -> f e1)
           -> BiGraph' key0 vA0 key1 vA1 e0
           -> f (BiGraph' key0 vB0 key1 vB1 e1)
acrossBoth f g h (Graph lf rt ed) = Graph <$> (arVals.traverse) f lf 
                                        <*> (arVals.traverse) g rt 
                                        <*> traverse h ed
mapKeys :: Ord k1
        => (k0 -> k1)
        -> BiGraph k0 vA vB
        -> BiGraph k1 vA vB
mapKeys f g = Graph 
        (mapALKey f $ g^.leftAL) 
        (mapALKey f $ g^.rightAL) 
        (g^.edges)

mapBoth :: (vA0 -> vB0)
        -> (vA1 -> vB1)
        -> BiGraph' key0 vA0 key1 vA1 e
        -> BiGraph' key0 vB0 key1 vB1 e
mapBoth f g = mapLeft f . mapRight g

mapBothWithKey :: (key -> vA0 -> vB0)
               -> (key -> vA1 -> vB1)
               -> BiGraph key vA0 vA1 
               -> BiGraph key vB0 vB1
mapBothWithKey f g = mapLeftWithKey f . mapRightWithKey g

mapEdges :: (eA -> eB) -> BiGraph' k0 v0 k1 v1 eA -> BiGraph' k0 v0 k1 v1 eB
mapEdges = over traverseEdges . (. view _3)

leftMap :: (Ord key0) => BiGraph' key0 v0 key1 v1 e -> HashMap key0 v0
leftMap g = M.fromList $ L.zip (A.elems $ g^.leftAL.arKey) (A.elems $ g^.leftAL.arVals)

rightMap :: Ord key1 => BiGraph' key0 v0 key1 v1 e -> HashMap key1 v1
rightMap g = M.fromList $ L.zip (A.elems $ g^.rightAL.arKey) (A.elems $ g^.rightAL.arVals)

edgeMap :: (Ord key0,Ord key1) => BiGraph' key0 v0 key1 v1 e -> HashMap (key0,key1) e
edgeMap g = M.mapKeys (f leftAL *** f rightAL) $ g^.edges
    where
        f ln = ((g^.ln.arKey) !)

instance ( Show key0, Show key1
         , Show e
         , Ord key0, Ord key1
         , Show v0, Show v1) 
        => Show (BiGraph' key0 v0 key1 v1 e) where
    show g = [s|Graph { left = %s, right = %s, edges = %s }|]
                (show $ leftMap g) 
                (show $ rightMap g) 
                (show $ edgeMap g)

insertEdge :: Ord key 
           => key -> v0 -> key -> v1 
           -> BiGraph key v0 v1 
           -> BiGraph key v0 v1
insertEdge kx x ky y g = g & leftAL  %~ f nx kx x ny
                          & rightAL %~ f ny ky y nx
                          & edges   %~ M.insert (nx,ny) ()
    where
        keyLookup ln k = fromMaybe (size $ g^.ln.mapKey) $ k `M.lookup` (g^.ln.mapKey)
        nx = keyLookup leftAL kx
        ny = keyLookup rightAL ky
        -- f :: Int -> key -> val -> Int 
        --   -> AdjList key val 
        --   -> AdjList key val
        arLU ar i f x 
            | inRange (bounds ar) i = f x $ ar ! i
            | otherwise             = x

        insertArray n x ar = array (second (max n) (bounds ar)) (A.assocs ar ++ [(n,x)])
        f n k x n' = (arKey   %~ insertArray n k)
                   . (arVals  %~ insertArray n x)
                   . (arEdges %~ (\ar -> insertArray n (arLU ar n (<>) (n' :| [])) ar))
                   . (mapKey  %~ M.insert k n)



readGraph :: BiGraph' key0 v0 key1 v1 e -> (forall s0 s1. GraphReader' key0 v0 key1 v1 e s0 s1 a) -> a
readGraph g (GR cmd) = runReader cmd g

forwardEdges :: Ord key0 => BiGraph' key0 v0 key1 v1 e -> HashMap key0 (v0,NonEmpty key1)
forwardEdges g = readGraph g $ do
    vs <- getLeftVertices
    xs <- mapM leftKey vs
    ys <- mapM leftInfo vs
    zs <- forM vs $ \v -> do
        es <- fmap target <$> successors v
        T.mapM rightKey es
    return $ M.fromList $ L.zip xs $ L.zip ys zs

backwardEdges :: (Ord key0,Ord key1) => BiGraph' key0 v0 key1 v1 e -> HashMap key1 (v1,NonEmpty key0)
backwardEdges g = readGraph g $ do
    vs <- getRightVertices
    xs <- mapM rightKey vs
    ys <- mapM rightInfo vs
    zs <- forM vs $ \v -> do
        es <- fmap source <$> predecessors v
        T.mapM leftKey es
    return $ M.fromList $ L.zip xs $ L.zip ys zs

hasLeftVertex :: Ord key0 => key0 -> GraphReader' key0 v0 key1 v1 e s0 s1 (Maybe (Vertex s0))
hasLeftVertex v = GR $ do
    vs <- view $ leftAL.mapKey
    return $ Vertex <$> v `M.lookup` vs

hasRightVertex :: Ord key1 => key1 -> GraphReader' key0 v0 key1 v1 e s0 s1 (Maybe (Vertex s1))
hasRightVertex v = GR $ do
    vs <- view $ rightAL.mapKey
    return $ Vertex <$> v `M.lookup` vs

leftVertex :: (Ord key0,Pre)
           => key0
           -> GraphReader' key0 v0 key1 v1 e s0 s1 (Vertex s0)
leftVertex v = GR $ do
    vs <- view $ leftAL.mapKey
    return $ Vertex $ fromJust' $ v `M.lookup` vs

getEdges :: GraphReader' key0 v0 key1 v1 e s0 s1 (HashMap (Edge s0 s1) ())
getEdges = GR $ do
    es <- views edges $ M.mapKeys (uncurry Edge)
    return $ () <$ es

rightVertex :: (Ord key1,Pre)
            => key1
            -> GraphReader' key0 v0 key1 v1 e s0 s1 (Vertex s1)
rightVertex v = GR $ do
    vs <- view $ rightAL.mapKey
    return $ Vertex $ fromJust' $ v `M.lookup` vs

edgeInfo :: Edge s0 s1 -> GraphReader' key0 v0 key1 v1 e s0 s1 e
edgeInfo (Edge i j) = GR $ views edges (! (i,j))

member :: (Ord key0,Ord key1) => key0 -> key1 
       -> BiGraph' key0 v0 key1 v1 e -> Bool
member k0 k1 g = isJust $ lookup k0 k1 g

leftMember :: Ord key0 => key0 
           -> BiGraph' key0 v0 key1 v1 e -> Bool
leftMember k0 g = isJust $ leftLookup k0 g

rightMember :: Ord key1 => key1 
            -> BiGraph' key0 v0 key1 v1 e -> Bool
rightMember k1 g = isJust $ rightLookup k1 g

lookup :: (Ord key0, Ord key1) 
       => key0 -> key1 
       -> BiGraph' key0 v0 key1 v1 e -> Maybe e
lookup k0 k1 g = readGraph g $ do
    v0 <- hasLeftVertex k0
    v1 <- hasRightVertex k1
    traverse edgeInfo =<< fmap join (sequence $ liftM2 hasEdge v0 v1)
    --traverse edgeInfo =<< hasEdge v0 v1

leftLookup :: Ord key0 
           => key0 -> BiGraph' key0 v0 key1 v1 e 
           -> Maybe v0
leftLookup k0 g = readGraph g $ do
    traverse leftInfo =<< hasLeftVertex k0

rightLookup :: Ord key1 => key1 
            -> BiGraph' key0 v0 key1 v1 e -> Maybe v1
rightLookup k1 g = readGraph g $ do
    traverse rightInfo =<< hasRightVertex k1

hasEdge :: Vertex s0 -> Vertex s1 -> GraphReader' key0 v0 key1 v1 e s0 s1 (Maybe (Edge s0 s1))
hasEdge (Vertex u) (Vertex v) = GR $ do
    es <- view edges
    -- Graph _ eL _ _ <- ask
    if (u,v) `M.member` es
    then return $ Just $ Edge u v
    else return Nothing

getLeftVertices :: GraphReader' key0 v0 key1 v1 e s0 s1 [Vertex s0]
getLeftVertices = GR $ do
    ar <- view $ leftAL.arKey
    -- Graph ar _ _ _ <- ask
    let (_,n) = bounds ar
    return $ L.map Vertex [0..n]

successors :: Vertex s0 -> GraphReader' key0 v0 key1 v1 e s0 s1 (NonEmpty (Edge s0 s1))
successors (Vertex u) = GR $ do
    ln <- view $ leftAL.arEdges
    return $ NE.map (Edge u) $ ln ! u

leftKey :: Vertex s0 -> GraphReader' key0 v0 key1 v1 e s0 s1 key0
leftKey (Vertex v) = GR $ do
    ar <- view $ leftAL.arKey
    return $ ar ! v

leftInfo :: Vertex s0 -> GraphReader' key0 v0 key1 v1 e s0 s1 v0
leftInfo (Vertex v) = GR $ do
    ar <- view $ leftAL.arVals
    return $ ar ! v

getRightVertices :: GraphReader' key0 v0 key1 v1 e s0 s1 [Vertex s1]
getRightVertices = GR $ do
    ar <- view $ rightAL.arKey
    -- Graph ar _ _ _ <- ask
    let (_,n) = bounds ar
    return $ L.map Vertex [0..n]

predecessors :: Vertex s1 -> GraphReader' key0 v0 key1 v1 e s0 s1 (NonEmpty (Edge s0 s1))
predecessors (Vertex u) = GR $ do
    AList _ _ ln _ <- view rightAL
    return $ NE.map (`Edge` u) $ ln ! u

rightKey :: Vertex s1 -> GraphReader' key0 v0 key1 v1 e s0 s1 key1
rightKey (Vertex v) = GR $ do
    AList ar _ _ _ <- view rightAL
    return $ ar ! v

rightInfo :: Vertex s1 -> GraphReader' key0 v0 key1 v1 e s0 s1 v1
rightInfo (Vertex v) = GR $ do
    AList _ ar _ _ <- view rightAL
    return $ ar ! v

transpose :: BiGraph' key0 v0 key1 v1 e -> BiGraph' key1 v1 key0 v0 e
transpose (Graph arL arR es) = Graph arR arL $ M.mapKeys swap es

source :: Edge s0 s1 -> Vertex s0
source (Edge v _) = Vertex v

target :: Edge s0 s1 -> Vertex s1
target (Edge _ v) = Vertex v

origins :: Edge s0 s1 -> (Vertex s0,Vertex s1)
origins (Edge v0 v1) = (Vertex v0, Vertex v1)

#if MIN_VERSION_transformers(0,5,0)
liftShowsListPrec :: (Int -> a -> ShowS)
                  -> Int -> [a] -> ShowS 
liftShowsListPrec showA _ xs = showString "[" . liftShowsListPrec' showA xs . showString "]"
-- liftShowsListPrec showA n = liftShowsPrec showA (liftShowsListPrec showA n) n
-- liftShowsListPrec showA n (x:xs) = showString "[]"

liftShowsListPrec' :: (Int -> a -> ShowS)
                   -> [a] -> ShowS 
-- liftShowsListPrec showA n = liftShowsPrec showA (liftShowsListPrec showA n) n
liftShowsListPrec' _showA [] = id
liftShowsListPrec' showA [x] = showA 0 x
liftShowsListPrec' showA (x:xs) = showA 0 x . showString "," . liftShowsListPrec' showA xs

liftShowsPairPrec :: (Int -> a -> ShowS)
                  -> (Int -> b -> ShowS)
                  -> Int -> (a,b) -> ShowS 
liftShowsPairPrec showA showB _ (x,y) = 
              showString "(" 
            . showA 0 x 
            . showString ", " 
            . showB 0 y 
            . showString ")"

liftShowsMap :: (Int -> k -> ShowS)
             -> (Int -> a -> ShowS)
             -> Int
             -> HashMap k a
             -> ShowS 
liftShowsMap showK showA n m = showParen (n >= 10) $ 
      showString "fromList " . liftShowsListPrec 
          (liftShowsPairPrec showK showA) 
          10 (M.toList m)

liftEqGraph :: (Ord k0,Ord k1,Ord k0',Ord k1')
               => (k0 -> k0' -> Bool)
               -> (v0 -> v0' -> Bool)
               -> (k1 -> k1' -> Bool)
               -> (v1 -> v1' -> Bool)
               -> (e -> e' -> Bool)
               -> BiGraph' k0 v0 k1 v1 e 
               -> BiGraph' k0' v0' k1' v1' e' 
               -> Bool
liftEqGraph eqK0 eqV0 eqK1 eqV1 eqE g g' = 
         liftEqMap eqK0 eqV0 (leftMap g) (leftMap g')
      && liftEqMap eqK1 eqV1 (rightMap g) (rightMap g')
      && liftEqMap (liftEqPair eqK0 eqK1) eqE (edgeMap g) (edgeMap g')

liftEqMap :: (k -> k' -> Bool)
          -> (a -> a' -> Bool)
          -> HashMap k a
          -> HashMap k' a'
          -> Bool
liftEqMap eqK eqA m0 m1 = liftEq
        (liftEqPair eqK eqA)
        (M.toList m0) 
        (M.toList m1)

liftEqPair :: (a -> a' -> Bool)
           -> (b -> b' -> Bool)
           -> (a,b)
           -> (a',b')
           -> Bool
liftEqPair f g (x,x') (y,y') = f x y && g x' y'

liftShowsGraph :: (Ord k0,Ord k1)
               => (Int -> k0 -> ShowS)
               -> (Int -> v0 -> ShowS)
               -> (Int -> k1 -> ShowS)
               -> (Int -> v1 -> ShowS)
               -> (Int -> e -> ShowS)
               -> Int
               -> BiGraph' k0 v0 k1 v1 e 
               -> ShowS
liftShowsGraph _showK0 _showV0 _showK1 _showV1 _showE _n _g
      = id
      -- = showParen (n >= 10) $
      --   showString $ [s|Graph { left = %s, right = %s, edges = %s }|]
      --           (liftShowsMap showK0 showV0 0 (leftMap g) "") 
      --           (liftShowsMap showK1 showV1 0 (rightMap g) "") 
      --           (liftShowsMap (liftShowsPairPrec showK0 showK1) showE 0 (edgeMap g) "") 

#endif

instance (NFData k,NFData a) => NFData (AdjList k a)
instance (NFData k0,NFData k1,NFData v0,NFData v1,NFData e) => NFData (BiGraph' k0 v0 k1 v1 e)

instance (Ord k0,Ord k1,ZoomEq k0,ZoomEq k1,ZoomEq v0,ZoomEq v1,ZoomEq e) 
        => ZoomEq (BiGraph' k0 v0 k1 v1 e) where
    g0 .== g1 = (edgeList g0) .== (edgeList g1)

instance (Ord v0,Serialize v0,Serialize v1,Serialize e) => Serialize (BiGraph v0 v1 e) where
instance (Ord v0,Serialize v0,Serialize v1) => Serialize (AdjList v0 v1) where
