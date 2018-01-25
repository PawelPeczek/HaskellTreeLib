{-# LANGUAGE TemplateHaskell #-}
module AVLTreeTest.ParametricTests where

import Test.QuickCheck.All
import Test.QuickCheck
import Utils
import AVLTree
import AVLTree.Internal
import Data.List (any, sort)
import Text.Show.Functions

-- | Instance allowing generation of random trees
instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (AVLTree a b) where
    arbitrary = sized $ (\n -> do
            pairs <- vector n
            return $ foldr (&:) newTree pairs
        )

-- Checks imdepotence of insertUnique
prop_insertUnique_imdepotence k v t =
    classify (t == newTree) "empty tree" $
    (insertUnique k v $ insertUnique k v t) == (insertUnique k v t)

-- Checks that genereted tree has balance coefficients correctly computed
prop_insertGeneratesValidTree = isValid . fromList

-- Checks that generated tree fulfills the AVL tree property
prop_insertGeneratesTreeWithValidHeight = isValid' . fromList

-- Checks that the binary t ree is balanced, ie. its depth is bounded
-- by log(2, n) where n is number of nodes
prop_depthIsBoundedByLog xs = length xs > 0 ==>
    depth (fromList xs) <= bound
    where bound = 1 + max 0 (ceiling $ logBase 2 (1.0 + (fromIntegral $ length xs)))

-- Checks that deletion preserves valid tree
prop_deleteGeneratesValidTree xs n = n >= 0 && n < length xs ==>
        let (toDel, _) = xs!!n  in
        isValid . fst . delete toDel $ fromList xs

-- Checks that deletion preserves tree fulfilling the AVL tree property
prop_deleteGeneratesValidDepthTree xs n = n >= 0 && n < length xs ==>
        let (toDel, _) = xs!!n  in
        isValid' . fst . delete toDel $ fromList xs

-- Checks that AVLTree treated a functor preserves identity
prop_functorIdentity xs f =
    (fmap f (fromList xs)) == (fromList $ map (\(k, v) -> (k, f v)) xs)

-- Checks that AVLTree treated a functor preserves function composition
prop_functorComposition xs f g =
    let tree = fromList xs in
    (fmap (f . g) $ tree) == (fmap f . fmap g $ tree)

-- Insert operator (&:) should be right-associative
prop_insertRightAssociativity kv1 kv2 tree =
    (kv1 &: kv2 &: tree) == (kv1 &: (kv2 &: tree))

-- Checks that function `keys` returns ordered values
prop_keysAreOrdered = isOrdered . keys

-- Checks that values retrieved from tree by key 
-- match expected (inserted) values
prop_valuesCanBeRetrieved xs = areKeysUnique xs ==>
    let tree = fromList xs in
    all (\(k, v) -> Just v == getValueByKey k tree) xs

-- Checks that function `values` returnes values in keys order
prop_valuesAreKeyOrdered xs = areKeysUnique xs ==>
    (values $ fromList xs) == (map snd . sort $ xs)

-- Checks that keys order is still valid after left rotation
prop_leftRotationPreservesOrdering xs =
    isValidInput tree ==>
    isOrdered . keys . leftRotation $ tree
    where tree = fromList xs
          isValidInput EmptyNode = False
          isValidInput (AVLNode _ _ _ EmptyNode _) = False
          isValidInput _ = True

-- Checks that keys order is still valid after left-left rotation
prop_llRotationPreservesOrdering xs =
    isValidInput tree ==>
    isOrdered . keys . llRotation $ tree
    where tree = fromList xs
          isValidInput EmptyNode = False
          isValidInput (AVLNode _ _ _ EmptyNode _) = False
          isValidInput _ = True

-- Checks that keys order is still valid after right rotation
prop_rightRotationPreservesOrdering xs =
    isValidInput tree ==>
    isOrdered . keys . rightRotation $ tree
    where tree = fromList xs
          isValidInput EmptyNode = False
          isValidInput (AVLNode _ _ EmptyNode _ _) = False
          isValidInput _ = True

-- Checks that keys order is still valid after left-right rotation
prop_lrRotationPreservesOrdering xs =
    isValidInput tree ==>
    isOrdered . keys . lrRotation $ tree
    where tree = fromList xs
          isValidInput EmptyNode = False
          isValidInput (AVLNode _ _ _ EmptyNode _) = False
          isValidInput (AVLNode _ _ _ (AVLNode _ _ EmptyNode _ _) _) = False
          isValidInput _ = True

-- Checks that keys order is still valid after right-left rotation
prop_rlRotationPreservesOrdering xs =
    isValidInput tree ==>
    isOrdered . keys . rlRotation $ tree
    where tree = fromList xs
          isValidInput EmptyNode = False
          isValidInput (AVLNode _ _ EmptyNode _ _) = False
          isValidInput (AVLNode _ _ (AVLNode _ _ _ EmptyNode _) _ _) = False
          isValidInput _ = True

-- Checks that keys order is still valid after right-right rotation
prop_rrRotationPreservesOrdering xs =
    isValidInput tree ==>
    isOrdered . keys . rrRotation $ tree
    where tree = fromList xs
          isValidInput EmptyNode = False
          isValidInput (AVLNode _ _ EmptyNode _ _) = False
          isValidInput _ = True

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
