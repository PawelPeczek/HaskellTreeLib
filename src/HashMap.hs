{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : HashMap
Description : Module with HashMap implemented on AVL tree
Copyright   : (c) Wojciech Geisler, 2018
                  Paweł Pęczek, 2018
-}

module HashMap
    (
      newHashMap,
      insertKeyVal,
      getElement,
      getKeys,
      getValues,
      HashMap.containsKey,
      deleteFromHM
    ) where

import HashKey
import AVLTree
import Data.Hashable

-- | Definition of basic structure used in HashMap
data HashMap a b = HashMap (AVLTree (HashKey a) b)

instance (Eq a, Hashable a, Show a, Show b) => Show (HashMap a b) where
    show hs = show . map (\k -> (k, getElement k hs)) $ getKeys hs

-- | Function that returns new empty 'HashMap'
newHashMap :: HashMap a b
newHashMap = HashMap newTree

-- | Function that inserts key and value into the given 'HashMap'
insertKeyVal :: (Eq a, Hashable a) =>
  a -- ^ key
  -> b -- ^ value
  -> HashMap a b -- ^ 'HashMap a b' to insert to
  -> HashMap a b -- ^ 'HashMap a b' after insertion
insertKeyVal k v (HashMap avl) = HashMap (insertUnique (prepareKey k) v avl)

-- | Function returns element with a given key from given HashMap
-- without deleting it. If given key is not present in the map Nothing
-- is returned.
getElement :: (Eq a, Hashable a) =>
  a -- ^ key of type a
  -> HashMap a b -- ^ 'HashMap a b' to search to
  -> Maybe b -- ^ result as described above
getElement k (HashMap avl) = getValueByKey (prepareKey k) avl

-- | Function that returns key set from HashMap - the order is random
getKeys :: (Eq a) =>
  HashMap a b -- ^ 'HashMap a b' to take keys
  -> [a] -- ^ set of keys
getKeys (HashMap avl) = loop (keys avl) []
  where
    loop [] acc = acc
    loop (x:xs) acc = loop xs ((getOriginalKey x) : acc)

-- | Function that returns values set in given HashMap - the order is random
getValues :: (Eq a) =>
  HashMap a b -- ^ 'HashMap a b' to take keys
  -> [b] -- ^ set of keys
getValues (HashMap avl) = loop (values avl) []
    where
      loop [] acc = acc
      loop (x:xs) acc = loop xs (x : acc)

-- | Function that chechs whether HashMap contains given key
containsKey :: (Hashable a, Eq a) =>
  a -- ^ Key of type a to check
  -> HashMap a b -- ^ given 'HashMap a b'
  -> Bool -- ^ result of check
containsKey k (HashMap avl) = AVLTree.containsKey (prepareKey k) avl

-- | Function deletes key and value from given HashMap and returns
-- pair (HashMap, Maybe elem) - so in case of success - the HashMap after
-- delete operation and deleted value, or otherwise pair (HashMap, Nothing)
-- with unmodified HashMap
deleteFromHM :: (Hashable a, Eq a) =>
  a -- ^ Key of type a to delete
  -> HashMap a b -- ^ given 'HashMap a b'
  -> (HashMap a b, Maybe b) -- ^ output as described above
deleteFromHM k (HashMap avl) =
  case delV of
    Nothing -> (HashMap avl', Nothing)
    _ -> (HashMap avl', delV)
  where
    (avl', delV) = delete (prepareKey k) avl
