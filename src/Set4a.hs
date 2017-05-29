{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4a where

import MCPrelude 
import Set4

-- Set1

randEven :: Gen Integer -- the output of rand * 2
randEven = Gen $ \s -> let (r, s') = rand s in (2 * r, s')

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = Gen $ \s -> let (r, s') = rand s in (2 * r + 1, s')

randTen :: Gen Integer -- the output of rand * 10
randTen = Gen $ \s -> let (r, s') = rand s in (10 * r, s')

generalA :: (a -> b) -> Gen a -> Gen b
generalA f ga = bind ga (\x -> return $ f x)

randEvenA :: Gen Integer -- the output of rand * 2
randEvenA = generalA (* 2) $ Gen rand

randOddA :: Gen Integer -- the output of rand * 2 + 1
randOddA = generalA ((+ 1) . (* 2)) $ Gen rand

randTenA :: Gen Integer -- the output of rand * 10
randTenA = generalA (* 10) $ Gen rand

randLetter :: Seed -> (Char, Seed)
randLetter s =
  let (x, newSeed) = rand s in
    (toLetter x, newSeed)
    
--  evalGen randPair (mkSeed 1)
randPair :: Gen (Char, Integer)
randPair = bind (Gen randLetter) (\rc -> bind (Gen rand) (\ri -> return (rc, ri)))

-- evalGen (generalPair (Gen randLetter) (Gen rand)) $ mkSeed 1 
generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb = bind ga (\rc -> bind gb (\ri -> return (rc, ri)))

-- evalGen (generalPair2 (Gen randLetter) (Gen rand)) $ mkSeed 1 
generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = liftM2 (,)

-- evalGen (repRandom (replicate 3 $ Gen randLetter)) (mkSeed 1)
repRandom :: [Gen a] -> Gen [a]
repRandom = sequence

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo = bind

mkGen :: a -> Gen a
mkGen = return

-- Set 2

addSalaries2' :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2' sis s1 s2 = liftM2 (+) (lookupMay s1 sis) (lookupMay s2 sis) 

transMaybe' :: Num a => ([a] -> a) -> [a] -> Maybe a
transMaybe' f xs = bind (tailMay xs) (\xs' -> return $ f xs)

-- Set 3

data Card = Card Int String

instance Show Card where
  show (Card x y) = show x ++ y

allPairs1 :: [a] -> [b] -> [(a,b)]
allPairs1 = liftM2 (,)

allCards1 :: [Int] -> [String] -> [Card]
allCards1 = liftM2 Card

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f xs ys = ap (map f xs) ys

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f xs ys zs = ap (ap (map f xs) ys) zs

