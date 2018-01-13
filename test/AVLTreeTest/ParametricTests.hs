{-# LANGUAGE TemplateHaskell #-}
module AVLTreeTest.ParametricTests where

import Test.QuickCheck.All
import Test.QuickCheck
import AVLTree

prop_foo x y = x + y == y + x

--newtype (Ord a) => OrdTree a b = OrdTree (AVLTree a b)

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (AVLTree a b) where
    arbitrary = sized $ (\n -> do
            keys <- vector n -- :: Gen [a]
            vals <- vector n -- :: Gen [b]
            return $ (foldr (&:) newTree (zip keys vals))
        )


prop_singletonInsert_imdepotence k v t =
    classify (t == newTree) "empty tree" $
    (insertSingleton k v $ insertSingleton k v t) == (insertSingleton k v t)

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
