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

--instance (Ord a, Arbitrary b) => Arbitrary OrdTree a b where
--instance Arbitrary (AVLTree Int b) where
--    arbitrary = sized $ (\n -> do
--            keys <- vector n :: Gen [a]
--            vals <- vector n :: Gen [b]
--            return $ OrdTree $ (foldr (&:) newTree (zip keys vals))
--        )
--        --return $ foldr (&:) newTree xs
--
newtype IntTree  = IntTree (AVLTree Int Int) deriving Show

newtype Tree2 b = Tree2 (AVLTree Int b) deriving Show

--instance (Arbitrary b) => Arbitrary (Tree2 b) where
--    --spec arbitrary :: Gen IntTree
--    arbitrary = sized $ (\n -> do
--            keys <- vector n -- :: Gen [Int]
--            vals <- vector n -- :: Gen [b]
--            return $ Tree2 (foldr (&:) newTree (zip keys vals)))


instance Arbitrary (IntTree) where
    --spec arbitrary :: Gen IntTree
    arbitrary = sized $ (\n -> do
        --xs <- arbitrary :: Gen [(a, b)]
            keys <- vector n :: Gen [Int]
            vals <- vector n :: Gen [Int]
            --vals <- vector n arbitrary
            return $ IntTree (foldr (&:) newTree (zip keys vals))
        )
        --return $ foldr (&:) newTree xs

--instance (Arbitrary b) => Arbitrary (IntTree b) where
--    --spec arbitrary :: Gen IntTree
--    arbitrary = sized $ (\n -> do
--        --xs <- arbitrary :: Gen [(a, b)]
--            keys <- vector n :: Gen [Int]
--            vals <- vector n :: Gen [b]
--            --vals <- vector n arbitrary
--            return $ IntTree (foldr (&:) newTree (zip keys vals))
--        )
--        --return $ foldr (&:) newTree xs

prop_insert_imdepotence k v =
    (insert k v $ insert k v $ newTree) == (insert k v $ newTree)

prop_singletonInsert_imdepotence k v =
    (insertSingleton k v $ insertSingleton k v $ newTree) == (insertSingleton k v $ newTree)

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
