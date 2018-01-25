{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : Examples
Description : Module with examples of library usage
Copyright   : (c) Wojciech Geisler, 2018
                  Paweł Pęczek, 2018
-}

module Examples where

import AVLSort
import TreeMap
import TreeSet

-- | First example of usage AVLSort module
avlSortExample1 :: IO () -- ^ IO () action as return value
avlSortExample1 = do
  putStrLn "Sorting list [1, 3, -2, 7, 15, 32, -3, 12, 9]"
  x <- return $ sortAVLAsc [1, 3, -2, 7, 15, 32, -3, 12, 9]
  putStrLn $ show $ x

-- | Second example of usage AVLSort module
avlSortExample2 :: IO () -- ^ IO () action as return value
avlSortExample2 = do
  putStrLn $ "Sorting list " ++ show [1..9]
  x <- return $ sortAVLAsc [1..9]
  putStrLn $ show $ x

-- | Third example of usage AVLSort module
avlSortExample3 :: IO () -- ^ IO () action as return value
avlSortExample3 = do
  putStrLn $ "Sorting list " ++ show (reverse [1..9])
  x <- return $ sortAVLAsc $ reverse [1..9]
  putStrLn $ show x

-- | Example of usage TreeMap module
treeMapExample1 :: IO () -- ^ IO () action as return value
treeMapExample1 = do
  putStrLn "Creating TreeMap from data: (Sławek 188), \
           \ (Wojtek 252), (Jan 388), (Dominik 38), \
           \ (Kaswery 88), (Jan 19) [reversed insertion order]"
  let tm = insertKeyVal "Slawek" 188  . insertKeyVal "Wojtek" 252
        . insertKeyVal "Jan" 388 . insertKeyVal "Diminik" 38
        . insertKeyVal "Ksawery" 88 . insertKeyVal "Jan" 19 $ newTreeMap
  putStrLn "Checking keys:"
  putStrLn . show $ getKeys tm
  putStrLn "Checking values:"
  putStrLn . show $ getValues tm
  putStrLn "Checking if TreeMap contains key Wojtek:"
  putStrLn . show $ containsKey "Wojtek" tm
  putStrLn "Deleting key Wojtek from TreeMap:"
  tm' <- return . fst $ deleteFromTM "Wojtek" tm
  putStrLn "Checking if TreeMap contains key Wojtek:"
  putStrLn . show $ containsKey "Wojtek" tm'
  putStrLn "Getting value associated to key Jan (Should be second value 388)"
  putStrLn . show $ getElement  "Jan" tm'

-- | Example of usage TreeSet module
treeSetExample1 :: IO ()  -- ^ IO () action as return value
treeSetExample1 = do
  putStrLn "Creating TreeMap from data: Słon, \
           \ Koza, Koza, Kon, Krowa, \
           \ Ciele, Kura [reversed insertion order]"
  ts <- return $ insertToSet "Slon" . insertToSet "Koza" . insertToSet "Koza"
               . insertToSet "Kon" . insertToSet "Krowa" . insertToSet "Ciele"
               . insertToSet "Kura" $ newTreeSet
  putStrLn "All elements in TreeSet:"
  putStrLn . show . getElements $ ts
  putStrLn "Checking if TreeSet contains element Kon"
  putStrLn . show $ containsElement "Kon" ts
  putStrLn "Checking if TreeSet contains element Zyrafa"
  putStrLn . show $ containsElement "Zyrafa" ts
  putStrLn "Deleting Krowa from TreeSet"
  ts' <- return . fst $ deleteElement "Krowa" ts
  putStrLn "All elements in TreeSet after deletion"
  putStrLn . show . getElements $ ts'
