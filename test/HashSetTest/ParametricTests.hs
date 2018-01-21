{-# LANGUAGE TemplateHaskell #-}
module HashSetTest.ParametricTests where

import Test.QuickCheck.All
import Test.QuickCheck
import Utils
import HashSet

prop_valuesInSetAreUnique xs = areUnique . getElements $ foldr (insertToSet) newHashSet xs
    where types = xs :: [Int]

prop_setContainsOnlyInserted vals excludes = let
    setVals = filter (\x -> not $ elem x excludes) vals
    newSet = foldr (insertToSet) newHashSet setVals in
    classify (setVals == []) "empty sets" $
    classify (excludes == []) "empty excluded" $
    (all (\x -> containsElement x newSet) setVals)
    &&
    (all (\x -> not $ containsElement x newSet) excludes)
    where types = (vals :: [Int], excludes :: [Int])

prop_deleteRemovesSetElements vals = let
    set = foldr (insertToSet) newHashSet (tail vals)
    results =
        (scanr (\val (deleted, correct, set) ->
            (val, False == containsElement deleted set, fst $ deleteElement val set))
        (head vals, True, set) vals)
    in areUnique vals ==>
    all (\(_, result, __) -> result) results
    where types = vals :: [Int]



--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
