{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : HashKey
Description : Module with HashKey that provide a way to keep elements in
              HashMap or HashSet
Copyright   : (c) Wojciech Geisler, 2018
                  Paweł Pęczek, 2018
-}

module HashKey
    (
      HashKey,
      prepareKey,
      getOriginalKey,
    ) where

import Data.Hashable
-- | Data type for holding keys in HashMap or HashSet
data HashKey a = HashKey (Int, a) deriving(Show)

-- | Instantiation of "Eq"
instance Eq a => Eq (HashKey a) where
  (==) (HashKey (i1, v1)) (HashKey (i2, v2)) = i1 == i2 && v1 == v2

-- | Instantiation of "Ord"
instance Eq a => Ord (HashKey a) where
  (<=) (HashKey (i1, v1)) (HashKey (i2, v2)) =
    i1 < i2 || (HashKey (i1, v1)) == (HashKey (i2, v2))

-- | Function that prepare hey
prepareKey :: (Hashable a, Eq a) =>
  a -- ^ Value to be hashed
  -> HashKey a -- ^ Key prepared to be used in HashMap/HashSet
prepareKey v = HashKey (hash v, v)

-- | Function that returns original key that was given as an argument of
-- function prepareKey
getOriginalKey ::
  HashKey a -- ^ hashed key to unpack
  -> a -- ^ Original key
getOriginalKey (HashKey (_, org)) = org
