{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : HashKey
Description : Module with HashKey that provide a way to keep elements in
              TreeMap or TreeSet
Copyright   : (c) Wojciech Geisler, 2018
                  Paweł Pęczek, 2018
-}

module HashKey
    (
      HashKey (HashKey),
      prepareKey,
      getOriginalKey,
      test
    ) where

import Data.Hashable
-- | Data type for holding keys in TreeMap or TreeSet
data HashKey a = HashKey (Int, a) deriving(Show)

-- | Instantiation of "Eq"
instance Eq a => Eq (HashKey a) where
  (==) (HashKey (i1, v1)) (HashKey (i2, v2)) = i1 == i2 && v1 == v2

-- | Instantiation of "Ord"
instance Eq a => Ord (HashKey a) where
  (<=) (HashKey (i1, v1)) (HashKey (i2, v2)) =
    i1 < i2 || (HashKey (i1, v1)) == (HashKey (i2, v2))

-- | Function preparing key to be used in a TreeMap
prepareKey :: (Hashable a, Eq a) =>
  a -- ^ Value to be hashed
  -> HashKey a -- ^ Key prepared to be used in TreeMap/TreeSet
prepareKey v = HashKey (hash v, v)

-- | Function that returns original key that was given as an argument of
-- function prepareKey
getOriginalKey ::
  HashKey a -- ^ hashed key to unpack
  -> a -- ^ Original key
getOriginalKey (HashKey (_, org)) = org

test = (prepareKey (5 :: Int)) == (HashKey (5,5))
