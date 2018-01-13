{-# LANGUAGE TemplateHaskell #-}
module AVLTreeTest.ParametricTests where

import Test.QuickCheck.All
import Test.QuickCheck
import AVLTree

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

prop_insertGeneratesValidTree = AVLTree.isValid . AVLTree.fromList

prop_insertGeneratesTreeWithValidHeight = AVLTree.isValid' . AVLTree.fromList

prop_deleteGeneratesValidTree xs = length xs /= 0 ==> do
        toDelIdx <- choose (0, length xs - 1)
        (toDel, _) <- return $ xs!!toDelIdx
        return $ AVLTree.isValid . fst . AVLTree.delete toDel $ AVLTree.fromList xs


prop_linearOrderIsOrdered = isOrdered . linearKeysOrder 

prop_rightRotationPreservesOrdering xs =
    isValidInput tree ==>
    isOrdered . linearKeysOrder . rightRotation $ tree
    where tree = fromList xs
          isValidInput EmptyNode = False
          isValidInput (AVLNode _ _ _ EmptyNode _) = False
          isValidInput _ = True

prop_rrRotationPreservesOrdering xs =
    isValidInput tree ==>
    isOrdered . linearKeysOrder . rrRotation $ tree
    where tree = fromList xs
          isValidInput EmptyNode = False
          isValidInput (AVLNode _ _ _ EmptyNode _) = False
          isValidInput _ = True

prop_leftRotationPreservesOrdering xs =
    isValidInput tree ==>
    isOrdered . linearKeysOrder . leftRotation $ tree
    where tree = fromList xs
          isValidInput EmptyNode = False
          isValidInput (AVLNode _ _ EmptyNode _ _) = False
          isValidInput _ = True

prop_rlRotationPreservesOrdering xs =
    isValidInput tree ==>
    isOrdered . linearKeysOrder . rlRotation $ tree
    where tree = fromList xs
          isValidInput EmptyNode = False
          isValidInput (AVLNode _ _ _ EmptyNode _) = False
          isValidInput (AVLNode _ _ _ (AVLNode _ _ EmptyNode _ _) _) = False
          isValidInput _ = True

prop_lrRotationPreservesOrdering xs =
    isValidInput tree ==>
    isOrdered . linearKeysOrder . lrRotation $ tree
    where tree = fromList xs
          isValidInput EmptyNode = False
          isValidInput (AVLNode _ _ EmptyNode _ _) = False
          isValidInput (AVLNode _ _ (AVLNode _ _ _ EmptyNode _) _ _) = False
          isValidInput _ = True

prop_llRotationPreservesOrdering xs =
    isValidInput tree ==>
    isOrdered . linearKeysOrder . llRotation $ tree
    where tree = fromList xs
          isValidInput EmptyNode = False
          isValidInput (AVLNode _ _ EmptyNode _ _) = False
          isValidInput _ = True

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
