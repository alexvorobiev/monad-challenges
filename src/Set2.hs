{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude 

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just x) = "Just " ++ show x

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay x =
  foldr (\(a, b) mb -> case mb of
            Nothing -> if x == a then Just b else Nothing
            jb -> jb) Nothing

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay x y = Just (x / y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay xs = Just $ foldl1 max xs

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay xs = Just $ foldl1 min xs

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd s =
  case lookupMay s gd of
    Nothing -> Nothing
    Just xs -> case tailMay xs of
      Nothing -> Nothing
      Just xs' -> case maximumMay xs' of
        Nothing -> Nothing
        Just mx -> case headMay xs of
          Nothing -> Nothing
          Just x -> divMay (fromIntegral mx) (fromIntegral x)

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain amb ma = case ma of
  Nothing -> Nothing
  Just a -> amb a

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

-- queryGreek2 greekDataB "chi"
queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gd s =
  link (lookupMay s gd) $ \xs ->
  link (tailMay xs) $ \xs' ->
  link (maximumMay xs') $ \mx ->
  link (headMay xs) $ \x ->
  divMay (fromIntegral mx) (fromIntegral x)

salaries :: [(String, Integer)]
salaries = [ ("alice", 105000)
           , ("bob", 90000)
           , ("carol", 85000)
           ]

-- addSalaries salaries "alice" "bob"
-- addSalaries salaries "alice" "jack"

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries sis s1 s2 =
  case lookupMay s1 sis of
    Nothing -> Nothing
    Just n1 -> case lookupMay s2 sis of
      Nothing -> Nothing
      Just n2 -> Just $ n1 + n2

yLink :: Maybe a -> Maybe b -> (a -> b -> c) -> Maybe c
yLink ma mb abc =
  case ma of
    Nothing -> Nothing
    Just a -> case mb of
      Nothing -> Nothing
      Just b -> Just $ abc a b

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 sis s1 s2 =
  yLink (lookupMay s1 sis) (lookupMay s2 sis) (+)

mkMaybe :: a -> Maybe a
mkMaybe = Just

tailProd :: Num a => [a] -> Maybe a
tailProd xs = case tailMay xs of
  Nothing -> Nothing
  Just xs' -> Just $ product xs'

tailSum :: Num a => [a] -> Maybe a
tailSum xs = case tailMay xs of
  Nothing -> Nothing
  Just xs' -> Just $ sum xs'

transMaybe :: Num a => ([a] -> a) -> [a] -> Maybe a
transMaybe f xs = case tailMay xs of
  Nothing -> Nothing
  Just xs' -> Just $ f xs'

tailProd2 :: Num a => [a] -> Maybe a
tailProd2 = transMaybe product

tailSum2 :: Num a => [a] -> Maybe a
tailSum2 = transMaybe sum

-- That's what they want 
transMaybe2 :: (a -> b) -> Maybe a -> Maybe b
transMaybe2 f ma =
  case ma of
    Nothing -> Nothing
    Just x -> Just $ f x

tailProd3 :: Num a => [a] -> Maybe a    
tailProd3 xs = transMaybe2 product $ tailMay xs

tailSum3 :: Num a => [a] -> Maybe a    
tailSum3 xs = transMaybe2 sum $ tailMay xs
  
tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax [] = Nothing
tailMax (_:xs) = Just $ if xs == [] then Nothing else
  Just $ foldr1 max xs 
  

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin [] = Nothing
tailMin (_:xs) = Just $ if xs == [] then Nothing else
  Just $ foldr1 min xs 

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing = Nothing
combine (Just Nothing) = Nothing
combine (Just (Just x)) = Just x
