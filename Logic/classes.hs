module Logic.Classes where

class Named n where
    name    :: n -> String
    as_pair :: n -> (String, n)
    as_pair n = (name n, n)
    
    decorated_name :: n -> String
    decorated_name = name

class Tree a where
    as_tree   :: a -> StrList
    rewriteM' :: Monad m => (b -> a -> m (b,a)) -> b -> a -> m (b,a)
    rewrite'  :: (b -> a -> (b,a)) -> b -> a -> (b,a)
    rewrite' f x t = (rewriteM' g x t) ()
        where
            g x t () = f x t
 
data StrList = List [StrList] | Str String

fold_mapM :: Monad m => (a -> b -> m (a,c)) -> a -> [b] -> m (a,[c])
fold_mapM _ s [] = return (s,[])
fold_mapM f s0 (x:xs) = do
        (s1,y)  <- f s0 x
        (s2,ys) <- fold_mapM f s1 xs
        return (s2,y:ys)

fold_map :: (a -> b -> (a,c)) -> a -> [b] -> (a,[c])
fold_map _ s [] = (s,[])
fold_map f s0 (x:xs) = (s2,y:ys)
    where
        (s1,y)  = f s0 x
        (s2,ys) = fold_map f s1 xs

visit :: Tree a => (b -> a -> b) -> b -> a -> b
visit f s x = fst $ rewrite' g s x
    where
        g s0 y = (f s0 y, y)

rewrite :: Tree a => (a -> a) -> a -> a
rewrite f x = snd $ rewrite' g () x
    where
        g () x = ((), f x)

visitM :: (Monad m, Tree a) => (b -> a -> m b) -> b -> a -> m b
visitM f x t = visit g (return x) t
    where
        g x t = do
            y <- x
            f y t

rewriteM :: (Monad m, Tree a) => (a -> m a) -> a -> m a
rewriteM f t = do
        ((),x) <- rewriteM' g () t
        return x
    where 
        g () x = do
            y <- f x
            return ((),y)
