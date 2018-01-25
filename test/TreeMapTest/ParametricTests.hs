{-# LANGUAGE TemplateHaskell #-}
module TreeMapTest.ParametricTests where

import Test.QuickCheck.All
import Test.QuickCheck
import Data.List
import Utils
import TreeMap

-- Checks that elements inserted to map can be retrieved by the key
prop_insertedElementCanBeFound vals = 
    let treemap = foldr (\(k, v) hs -> insertKeyVal k v hs) newTreeMap vals
    in areKeysUnique vals ==>
    all (\(k, v) -> Just v == getElement k treemap) vals

-- Checks that elements are overwritten upon insert with the same key
prop_laterInsertOverwritesValue keys = 
    areUnique keys ==> -- unexpected repeated keys should not interfere
    do
    vals1 <- vector (length keys) :: Gen [Int]
    vals2 <- vector (length keys) :: Gen [Int]
    let hs1 = foldr (\(k, v) hs -> insertKeyVal k v hs) newTreeMap $ zip keys vals1 
    let hs2 = foldr (\(k, v) hs -> insertKeyVal k v hs) hs1 $ zip keys vals2 
    return $ 
        (all (\(k, v) -> Just v == getElement k hs1) $ zip keys vals1)
        &&
        (all (\(k, v) -> Just v == getElement k hs2) $ zip keys vals2)

-- Checks that getValues returns all inserted values
prop_getValuesReturnsAll keys vals =
    areUnique keys ==>
    let
    used = take (length keys) vals
    hs = foldr (\(k, v) hs -> insertKeyVal k v hs) newTreeMap $ zip keys vals in
    (sort used) == (sort . getValues $ hs)


-- Checks that deleted element cannot be found in the TreeMap
prop_deleteRemovesElement [] = classify True "trivial" $ True
prop_deleteRemovesElement keyvals = let
    keys@(fstK:_) = fst . unzip $ keyvals
    fullHS = foldr (uncurry insertKeyVal) newTreeMap keyvals
    (_, result, finalHS) =
        (foldr (\k (deleted, correct, hs) ->
            (k, correct && not (containsKey deleted hs), fst $ deleteFromTM k hs))
        (fstK, True, fst $ deleteFromTM fstK fullHS) keys)
    -- bucket for test cases classification
    [down, up] = (10 *) <$> ([floor, ceiling] <*> [(fromIntegral $ length keyvals) / 10])
    in
    areKeysUnique keyvals ==>
    classify True ("length in " ++ show [down, up]) $
    result
    where types = keyvals :: [(String, Int)]


--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
