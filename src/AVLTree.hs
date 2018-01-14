{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : AVLTree
Description : Module with basic functionality of AVL Tree included
Copyright   : (c) Wojciech Geisler, 2018
                  Paweł Pęczek, 2018
-}
module AVLTree (
    AVLTree,
    newTree,
    insert,
    delete,
    linearOrder,
    linearKeysOrder
    ) where
--(
--          AVLTree,
--          newTree,
--          isValid,
--          fromList,
--          linearOrder,
--          reversedOrder,
--          linearKeysOrder,
--          reversedKeysOrder,
--          insert,
--          insertUnique,
--          insertKeyAsValue,
--          insertKeyAsValueUnique,
--          debugShow,
--          depth,
--          delete,
--          containsKey,
--          delete,
--          getValueOfKey,
--          (&:)
--        )
--        where

import AVLTree.Internal

-- | Infix operator for adding a key value pair to a tree
(&:) :: Ord a => (a, b) -> AVLTree a b -> AVLTree a b
(&:) (k, v) t = insert k v t

