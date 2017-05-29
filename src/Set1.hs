{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude 

fiveRands :: [Integer]
fiveRands = 
  fst $ foldr (\_ (xs, seed) -> let (x, newSeed) = rand seed in (x:xs, newSeed)) ([], mkSeed 1) [1..5]

randLetter :: Seed -> (Char, Seed)
randLetter s =
  let (x, newSeed) = rand s in
    (toLetter x, newSeed)

randString3 :: String
randString3 = 
  reverse . fst $ foldr (\_ (xs, seed) -> let (x, newSeed) = randLetter seed in (x:xs, newSeed)) ("", mkSeed 1) [1..3]
  
type Gen a = Seed -> (a, Seed)

randEven :: Gen Integer -- the output of rand * 2
randEven = \s -> let (r, s') = rand s in (2 * r, s')

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = \s -> let (r, s') = rand s in (2 * r + 1, s')

randTen :: Gen Integer -- the output of rand * 10
randTen = \s -> let (r, s') = rand s in (10 * r, s')

generalA :: (a -> b) -> Gen a -> Gen b
generalA f ga = \s -> let (r, s') = ga s in (f r, s')

randEvenA :: Gen Integer -- the output of rand * 2
randEvenA = generalA (* 2) rand

randOddA :: Gen Integer -- the output of rand * 2 + 1
randOddA = generalA ((+ 1) . (* 2)) rand

randTenA :: Gen Integer -- the output of rand * 10
randTenA = generalA (* 10) rand

--  randPair (mkSeed 1)
randPair :: Gen (Char, Integer)
randPair = \s -> let (rc, s') = randLetter s in
  let (ri, s'') = rand s' in ((rc, ri), s'')

--  generalPair randLetter rand $ mkSeed 1 
generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb =
  \s -> let (rc, s') = ga s in
    let (ri, s'') = gb s' in ((rc, ri), s'')

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f ga gb =
  \s -> let (rc, s') = ga s in
    let (ri, s'') = gb s' in ((f rc ri), s'')

--  generalPair2 randLetter rand $ mkSeed 1 
generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (,)

-- repRandom (replicate 3 randLetter) (mkSeed 1)
repRandom :: [Gen a] -> Gen [a]
repRandom = foldr (\ga gas -> (\s -> let (r, s') = ga s in
                                  let (rs, s'') = gas s' in (r:rs, s'')))
  (\s -> ([], s))

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo ga agb =
  \s -> let (r, s') = ga s in agb r s'

mkGen :: a -> Gen a
mkGen a = \s -> (a, s)

-- for Set4
generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f ga gb = genTwo ga (\r -> genTwo gb (\r' -> \s -> (f r r', s)))

generalPair2' :: Gen a -> Gen b -> Gen (a, b)
generalPair2' = generalB2 (,)
