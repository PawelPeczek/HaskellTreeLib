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
    .&&.
    (all (\x -> not $ containsElement x newSet) excludes)
    where types = (vals :: [Int], excludes :: [Int])

prop_deleteRemovesSetElements [] = classify True "trivial" $ True
prop_deleteRemovesSetElements vals@(v:als) = let
    set = foldr (insertToSet) newHashSet (als)
    (_, result, finalSet) =
        (foldr (\val (deleted, correct, set) ->
            (val, correct && not (containsElement deleted set), fst $ deleteElement val set))
        (v, True, set) vals)
    -- classification boundaries
    [down, up] = (10 *) <$> ([floor, ceiling] <*> [(fromIntegral $ length vals) / 10])
    in
    areUnique vals ==>
    classify True ("length in " ++ show [down, up]) $
    result -- && finalSet == newHashSet
    where types = vals :: [Int]



--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
