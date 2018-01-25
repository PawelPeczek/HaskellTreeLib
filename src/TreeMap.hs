{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : TreeMap
Description : Module with TreeMap implemented on AVL tree
Copyright   : (c) Wojciech Geisler, 2018
                  Paweł Pęczek, 2018
-}

module TreeMap
    (
      newTreeMap,
      insertKeyVal,
      getElement,
      getKeys,
      getValues,
      TreeMap.containsKey,
      deleteFromTM
    ) where

import HashKey
import AVLTree
import Data.Hashable

-- | Definition of basic structure used in TreeMap
data TreeMap a b = TreeMap (AVLTree (HashKey a) b)

instance (Eq a, Hashable a, Show a, Show b) => Show (TreeMap a b) where
    show tm = show . map (\k -> (k, getElement k tm)) $ getKeys tm

-- | Function that returns new empty 'TreeMap'
newTreeMap :: TreeMap a b
newTreeMap = TreeMap newTree

-- | Function that inserts key and value into the given 'TreeMap'
insertKeyVal :: (Eq a, Hashable a) =>
  a -- ^ key
  -> b -- ^ value
  -> TreeMap a b -- ^ 'TreeMap a b' to insert to
  -> TreeMap a b -- ^ 'TreeMap a b' after insertion
insertKeyVal k v (TreeMap avl) = TreeMap (insertUnique (prepareKey k) v avl)

-- | Function returns element with a given key from given TreeMap
-- without deleting it. If given key is not present in the map Nothing
-- is returned.
getElement :: (Eq a, Hashable a) =>
  a -- ^ key of type a
  -> TreeMap a b -- ^ 'TreeMap a b' to search to
  -> Maybe b -- ^ result as described above
getElement k (TreeMap avl) = getValueByKey (prepareKey k) avl

-- | Function that returns key set from TreeMap - the order is random
getKeys :: (Eq a) =>
  TreeMap a b -- ^ 'TreeMap a b' to take keys
  -> [a] -- ^ set of keys
getKeys (TreeMap avl) = loop (keys avl) []
  where
    loop [] acc = acc
    loop (x:xs) acc = loop xs ((getOriginalKey x) : acc)

-- | Function that returns values set in given TreeMap - the order is random
getValues :: (Eq a) =>
  TreeMap a b -- ^ 'TreeMap a b' to take keys
  -> [b] -- ^ set of keys
getValues (TreeMap avl) = loop (values avl) []
    where
      loop [] acc = acc
      loop (x:xs) acc = loop xs (x : acc)

-- | Function that chechs whether TreeMap contains given key
containsKey :: (Hashable a, Eq a) =>
  a -- ^ Key of type a to check
  -> TreeMap a b -- ^ given 'TreeMap a b'
  -> Bool -- ^ result of check
containsKey k (TreeMap avl) = AVLTree.containsKey (prepareKey k) avl

-- | Function deletes key and value from given TreeMap and returns
-- pair (TreeMap, Maybe elem) - so in case of success - the TreeMap after
-- delete operation and deleted value, or otherwise pair (TreeMap, Nothing)
-- with unmodified TreeMap
deleteFromTM :: (Hashable a, Eq a) =>
  a -- ^ Key of type a to delete
  -> TreeMap a b -- ^ given 'TreeMap a b'
  -> (TreeMap a b, Maybe b) -- ^ output as described above
deleteFromTM k (TreeMap avl) =
  case delV of
    Nothing -> (TreeMap avl', Nothing)
    _ -> (TreeMap avl', delV)
  where
    (avl', delV) = delete (prepareKey k) avl
