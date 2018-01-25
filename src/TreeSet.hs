{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : TreeSet
Description : Module with TreeSet implemented on AVL tree
Copyright   : (c) Wojciech Geisler, 2018
                  Paweł Pęczek, 2018
-}

module TreeSet
    (
      newTreeSet,
      insertToSet,
      getElements,
      containsElement,
      deleteElement
    ) where

import HashKey
import AVLTree
import Data.Hashable

-- | Definition of basic structure used in TreeSet
data TreeSet a = TreeSet (AVLTree (HashKey a) ()) deriving (Eq)

instance  (Show a, Eq a) => Show (TreeSet a) where
    show = show . getElements

-- | Function that returns new empty 'TreeSet'
newTreeSet :: TreeSet a
newTreeSet = TreeSet newTree

-- | Function that performs insert operation to TreeSet
insertToSet :: (Eq a, Hashable a) =>
  a -- ^ key
  -> TreeSet a -- ^ 'TreeSet a' to insert to
  -> TreeSet a -- ^ 'TreeSet a' after insertion
insertToSet e (TreeSet avl) = TreeSet (insertKeyAsValueUnique (prepareKey e) avl)

-- | Function that returns all elements from TreeSet - the order is random
getElements :: (Eq a) =>
  TreeSet a -- ^ 'TreeSet a' to take keys
  -> [a] -- ^ set of keys
getElements (TreeSet avl) =
  foldr (\k xs -> (getOriginalKey k) : xs) [] (keys avl)

-- | Function that chechs whether TreeSet contains given element
containsElement :: (Hashable a, Eq a) =>
  a -- ^ element of type a to check
  -> TreeSet a -- ^ given 'TreeSet a'
  -> Bool -- ^ result of check
containsElement k (TreeSet avl) = containsKey (prepareKey k) avl

-- | Function deletes key and value from given TreeSet and returns
-- pair (TreeSet, Maybe elem) - so in case of success - the TreeSet after
-- delete operation and deleted value, or otherwise pair (TreeSet, Nothing)
-- with unmodified TreeSet
deleteElement :: (Hashable a, Eq a) =>
  a -- ^ Key of type a to delete
  -> TreeSet a -- ^ given 'TreeSet a'
  -> (TreeSet a, Maybe a) -- ^ output as described above
deleteElement k (TreeSet avl) =
  case delV of
    Nothing -> (TreeSet avl', Nothing)
    _ -> (TreeSet avl', Just k)

  where
    (avl', delV) = delete (prepareKey k) avl
