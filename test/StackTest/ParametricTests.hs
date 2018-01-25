{-# LANGUAGE TemplateHaskell #-}
module StackTest.ParametricTests where

import Test.QuickCheck.All
import Test.QuickCheck
import Utils
import Stack

-- Checks that toList returnes values in reversed order of their insertion
-- (note the use of foldr)
prop_LIFO vals = (vals ==) . toList . foldr push emptyStack $ vals

-- Checks that stack is empty after number of `pop`s equal to number of `push`es
prop_stackIsEmptyAfterNPops vals =
    let stack = foldr push emptyStack vals in
    isEmpty $ foldl (\s _  -> fst $ pop s) stack [1..(length vals)]
    where types = vals :: [Int]

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
