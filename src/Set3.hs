{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude 

allPairs :: [a] -> [b] -> [(a,b)]
allPairs [] _ = []
allPairs _ [] = []

-- allPairs [1..3] [6..8]
allPairs (x:xs) ys = map ((,) x) ys ++ allPairs xs ys

data Card = Card Int String

instance Show Card where
  show (Card x y) = show x ++ y

-- show (allCards cardRanks cardSuits) == "[2H,2D,2C,2S,3H,3D,3C,3S,4H,4D,4C,4S,5H,5D,5C,5S]"
allCards :: [Int] -> [String] -> [Card]
allCards [] _ = []
allCards (x:xs) ys = map (Card x) ys ++ allCards xs ys

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ [] _ = []
allCombs f (x:xs) ys = map (f x) ys ++ allCombs f xs ys

allPairs1 :: [a] -> [b] -> [(a,b)]
allPairs1 = allCombs (,)

allCards1 :: [Int] -> [String] -> [Card]
allCards1 = allCombs Card

-- allCombs3 (,,) [1,2] [3,4] [5,6] == [(1,3,5),(1,3,6),(1,4,5),(1,4,6),(2,3,5),(2,3,6),(2,4,5),(2,4,6)]
allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 _ [] _ _ = []
allCombs3 f (x:xs) ys zs = (allCombs (f x) ys zs) ++ allCombs3 f xs ys zs

combStep :: [a -> b] -> [a] -> [b]
combStep = allCombs ($)

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f xs ys = combStep (map f xs) ys

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f xs ys zs = combStep (combStep (map f xs) ys) zs
