{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude 
import Set2

class Monad m where
  bind :: m x -> (x -> m y) -> m y
  return :: x -> m x
  fail :: x -> m x

generalB2m :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
generalB2m f ma mb = bind ma (\r -> bind mb (\r' -> return $ f r r'))

newtype Gen a = Gen (Seed -> (a, Seed))

instance Monad Gen where
  bind (Gen ga) agb = Gen $ \s -> let (r, s') = ga s in
    let (Gen gb) = agb r in gb s'
    
  return x = Gen $ \s -> (x, s)
  fail _ = Gen $ undefined

evalGen :: Gen a -> Seed -> a
evalGen (Gen sas) = fst . sas

instance Monad Maybe where
  bind = link
  return = mkMaybe
  fail _ = Nothing

instance Monad [] where
  bind xs xys = concat $ map xys xs
  return x = [x]
  fail _ = []
    
sequence :: Monad m => [m a] -> m [a]
sequence = foldr (\ma mas -> bind ma
                   (\x -> bind mas
                     (\xs -> return $ x:xs))) $ return []

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = bind ma (\x -> bind mb (\y -> return $ f x y))

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind

(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>>=) = bind

join :: Monad m => m (m a) -> m a
join mma = bind mma id 

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = bind ma (\x -> liftM2 (f x) mb mc)

ap :: Monad m => m (a -> b) -> m a -> m b
ap = liftM2 ($)

