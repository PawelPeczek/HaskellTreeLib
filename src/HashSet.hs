{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : HashSet
Description : Module with HashSet implemented on AVL tree
Copyright   : (c) Wojciech Geisler, 2018
                  Paweł Pęczek, 2018
-}

module HashSet
    (
      newHashSet,
      insertToSet,
      getElements,
      containsElement,
      deleteElement
    ) where

import HashKey
import AVLTree
import Data.Hashable

-- | Definition of basic structure used in HashSet
data HashSet a = HashSet (AVLTree (HashKey a) ())

-- | Function that returns new empty 'HashSet'
newHashSet :: HashSet a
newHashSet = HashSet newTree

-- | Function that performs insert operation to HashSet
insertToSet :: (Eq a, Hashable a) =>
  a -- ^ key
  -> HashSet a -- ^ 'HashSet a' to insert to
  -> HashSet a -- ^ 'HashSet a' after insertion
insertToSet e (HashSet avl) = HashSet (insertKeyAsValueUnique (prepeareKey e) avl)

-- | Function that returns all elements from HashSet - the order is random
getElements :: (Eq a) =>
  HashSet a -- ^ 'HashSet a' to take keys
  -> [a] -- ^ set of keys
getElements (HashSet avl) =
  foldr (\k xs -> (getOriginalKey k) : xs) [] (linearKeysOrder avl)

-- | Function that chechs whether HashSet contains given element
containsElement :: (Hashable a, Eq a) =>
  a -- ^ element of type a to check
  -> HashSet a -- ^ given 'HashSet a'
  -> Bool -- ^ result of check
containsElement k (HashSet avl) = containsKey (prepeareKey k) avl

-- | Function deletes key and value from given HashSet and returns
-- pair (HashSet, Maybe elem) - so in case of success - the HashSet after
-- delete operation and deleted value, or otherwise pair (HashSet, Nothing)
-- with unmodified HashSet
deleteElement :: (Hashable a, Eq a) =>
  a -- ^ Key of type a to delete
  -> HashSet a -- ^ given 'HashSet a'
  -> (HashSet a, Maybe ()) -- ^ output as described above
deleteElement k (HashSet avl) =
  case delV of
    Nothing -> (HashSet avl', Nothing)
    _ -> (HashSet avl', delV)
  where
    (avl', delV) = delete (prepeareKey k) avl
