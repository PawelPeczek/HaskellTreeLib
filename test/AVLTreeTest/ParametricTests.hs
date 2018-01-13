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


prop_insert_imdepotence k v =
    (insert k v $ insert k v $ newTree) == (insert k v $ newTree)

prop_singletonInsert_imdepotence k v =
    (insertSingleton k v $ insertSingleton k v $ newTree) == (insertSingleton k v $ newTree)

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
