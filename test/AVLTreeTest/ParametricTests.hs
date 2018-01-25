{-# LANGUAGE TemplateHaskell #-}
module AVLTreeTest.ParametricTests where

import Test.QuickCheck.All
import Test.QuickCheck
import Utils
import AVLTree
import AVLTree.Internal
import Data.List (any, sort)
import Text.Show.Functions

--newtype (Ord a) => OrdTree a b = OrdTree (AVLTree a b)

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (AVLTree a b) where
    arbitrary = sized $ (\n -> do
            pairs <- vector n
            return $ foldr (&:) newTree pairs
        )


prop_singletonInsert_imdepotence k v t =
    classify (t == newTree) "empty tree" $
    (insertUnique k v $ insertUnique k v t) == (insertUnique k v t)

prop_insertGeneratesValidTree = isValid . fromList

prop_insertGeneratesTreeWithValidHeight = isValid' . fromList

prop_depthIsBoundedByLog xs = length xs > 0 ==>
    depth (fromList xs) <= bound
    where bound = 1 + max 0 (ceiling $ logBase 2 (1.0 + (fromIntegral $ length xs)))

prop_deleteGeneratesValidTree xs n = n >= 0 && n < length xs ==>
        let (toDel, _) = xs!!n  in
        isValid . fst . delete toDel $ fromList xs

prop_deleteGeneratesValidDepthTree xs n = n >= 0 && n < length xs ==>
        let (toDel, _) = xs!!n  in
        isValid' . fst . delete toDel $ fromList xs

-- Functors must preserve identity morphisms
prop_functorIdentity xs f =
    (fmap f (fromList xs)) == (fromList $ map (\(k, v) -> (k, f v)) xs)

-- Functors preserve composition of morphisms 
prop_functorComposition xs f g =
    let tree = fromList xs in
    (fmap (f . g) $ tree) == (fmap f . fmap g $ tree)

-- Insert operator (&:) should be right-associative
prop_insertRightAssociativity kv1 kv2 tree =
    (kv1 &: kv2 &: tree) == (kv1 &: (kv2 &: tree))

prop_keysAreOrdered = isOrdered . keys

prop_associtedValuesAreFound xs = areKeysUnique xs ==>
    let tree = fromList xs in
    all (\(k, v) -> Just v == getValueByKey k tree) xs

prop_valuesAreKeyOrdered xs = areKeysUnique xs ==>
    (values $ fromList xs) == (map snd . sort $ xs)

prop_leftRotationPreservesOrdering xs =
    isValidInput tree ==>
    isOrdered . keys . leftRotation $ tree
    where tree = fromList xs
          isValidInput EmptyNode = False
          isValidInput (AVLNode _ _ _ EmptyNode _) = False
          isValidInput _ = True

prop_llRotationPreservesOrdering xs =
    isValidInput tree ==>
    isOrdered . keys . llRotation $ tree
    where tree = fromList xs
          isValidInput EmptyNode = False
          isValidInput (AVLNode _ _ _ EmptyNode _) = False
          isValidInput _ = True

prop_rightRotationPreservesOrdering xs =
    isValidInput tree ==>
    isOrdered . keys . rightRotation $ tree
    where tree = fromList xs
          isValidInput EmptyNode = False
          isValidInput (AVLNode _ _ EmptyNode _ _) = False
          isValidInput _ = True

prop_lrRotationPreservesOrdering xs =
    isValidInput tree ==>
    isOrdered . keys . lrRotation $ tree
    where tree = fromList xs
          isValidInput EmptyNode = False
          isValidInput (AVLNode _ _ _ EmptyNode _) = False
          isValidInput (AVLNode _ _ _ (AVLNode _ _ EmptyNode _ _) _) = False
          isValidInput _ = True

prop_rlRotationPreservesOrdering xs =
    isValidInput tree ==>
    isOrdered . keys . rlRotation $ tree
    where tree = fromList xs
          isValidInput EmptyNode = False
          isValidInput (AVLNode _ _ EmptyNode _ _) = False
          isValidInput (AVLNode _ _ (AVLNode _ _ _ EmptyNode _) _ _) = False
          isValidInput _ = True

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
