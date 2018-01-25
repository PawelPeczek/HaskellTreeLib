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
    getValueByKey
    ) where

import AVLTree.Internal

