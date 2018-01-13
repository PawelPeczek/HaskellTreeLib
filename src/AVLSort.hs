{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : AVLSort
Description : Module with basic AVL sort functionality
              Main feature: sorting in O(NlogN)
Copyright   : (c) Wojciech Geisler, 2018
                  Paweł Pęczek, 2018
-}

module AVLSort(
  sortAVLAsc
) where

import AVLTree

-- | Function performs sort at list of elements of type a (instance of "Ord")
-- using AVLTree to maintain the sorting process. Each of N elements is
-- inserting to the AVLTree in O(logN) time, so overally the time cost
-- of building the tree is O(NlogN) time and O(N) space. After building the tree
-- it is simply linearized in O(N) time.
-- Sorting order: ASCENDING
sortAVLAsc :: Ord a =>
  [a] --- ^ input list of elements of type a
  -> [a]-- ^ sorted (ASC) input list
sortAVLAsc [] = []
sortAVLAsc xs =
   linearKeysOrder . foldr insertKeyAsValue newTree $ xs
