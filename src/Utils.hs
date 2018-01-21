module Utils where

import Data.List (sort)

-- checks if a list is ordered
isOrdered :: (Ord a) => [a] -> Bool
isOrdered [] = True
isOrdered [_] = True
isOrdered (x:y:xs) = x <= y && isOrdered (y:xs)

-- checks if all elemeents of a list are unique
areUnique :: (Ord a) => [a] -> Bool
areUnique xs = let consecutive = zip <*> tail $ sort xs
    in all (uncurry (/=)) consecutive

-- checks if all keys in a list of key, value tuples are unique
areKeysUnique :: (Ord a) => [(a, b)] -> Bool
areKeysUnique = areUnique . map fst
