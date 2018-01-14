{-# LANGUAGE TemplateHaskell #-}
module AVLTreeTest.ParametricTests where

import Test.QuickCheck.All
import Test.QuickCheck
import AVLTree
import AVLTree.Internal

--newtype (Ord a) => OrdTree a b = OrdTree (AVLTree a b)

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (AVLTree a b) where
    arbitrary = sized $ (\n -> do
            pairs <- vector n
            return $ foldr (&:) newTree pairs
        )

isOrdered :: (Ord a) => [a] -> Bool
isOrdered [] = True
isOrdered [_] = True
isOrdered (x:y:xs) = x <= y && isOrdered xs

prop_singletonInsert_imdepotence k v t =
    classify (t == newTree) "empty tree" $
    (insertUnique k v $ insertUnique k v t) == (insertUnique k v t)

prop_insertGeneratesValidTree = AVLTree.Internal.isValid . AVLTree.fromList

prop_linearOrderIsOrdered = isOrdered . linearKeysOrder

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
