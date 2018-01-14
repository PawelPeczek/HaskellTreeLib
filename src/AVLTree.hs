{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : AVLTree
Description : Public module for AVL tree functionality.
Copyright   : (c) Wojciech Geisler, 2018
                  Paweł Pęczek, 2018
-}
module AVLTree (
    AVLTree,
    newTree,
    fromList,
    keys,
    values,
    insert,
    insertUnique,
    treeToList,
    insertKeyAsValue,
    insertKeyAsValueUnique,
    delete,
    containsKey,
    getValueOfKey
    ) where

import AVLTree.Internal

-- | Infix operator for adding a key value pair to a tree
(&:) :: Ord a => (a, b) -> AVLTree a b -> AVLTree a b
(&:) (k, v) t = insert k v t

instance Functor (AVLTree a) where
    fmap f EmptyNode = EmptyNode
    fmap f (AVLNode k v lt rt bc) =
        (AVLNode k (f v) (fmap f lt) (fmap f rt) bc)

