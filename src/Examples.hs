{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : Examples
Description : Module with examples of library usage
Copyright   : (c) Wojciech Geisler, 2018
                  Paweł Pęczek, 2018
-}

module Examples where

import AVLSort
import HashMap
import HashSet

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

-- | Example of usage HashMap module
hashMapExample1 :: IO () -- ^ IO () action as return value
hashMapExample1 = do
  putStrLn "Creating HashMap from data: (Sławek 188), \
           \ (Wojtek 252), (Jan 388), (Dominik 38), \
           \ (Kaswery 88), (Jan 19) [reversed insertion order]"
  let hm = insertKeyVal "Slawek" 188  . insertKeyVal "Wojtek" 252
        . insertKeyVal "Jan" 388 . insertKeyVal "Diminik" 38
        . insertKeyVal "Ksawery" 88 . insertKeyVal "Jan" 19 $ newHashMap
  putStrLn "Checking keys:"
  putStrLn . show $ getKeys hm
  putStrLn "Checking values:"
  putStrLn . show $ getValues hm
  putStrLn "Checking if HashMap contains key Wojtek:"
  putStrLn . show $ containsKey "Wojtek" hm
  putStrLn "Deleting key Wojtek from HashMap:"
  hm' <- return . fst $ deleteFromHM "Wojtek" hm
  putStrLn "Checking if HashMap contains key Wojtek:"
  putStrLn . show $ containsKey "Wojtek" hm'
  putStrLn "Getting value associated to key Jan (Should be second value 388)"
  putStrLn . show $ getElement  "Jan" hm'

-- | Example of usage HashSet module
hashSetExample1 :: IO ()  -- ^ IO () action as return value
hashSetExample1 = do
  putStrLn "Creating HashMap from data: Słon, \
           \ Koza, Koza, Kon, Krowa, \
           \ Ciele, Kura [reversed insertion order]"
  hs <- return $ insertToSet "Slon" . insertToSet "Koza" . insertToSet "Koza"
               . insertToSet "Kon" . insertToSet "Krowa" . insertToSet "Ciele"
               . insertToSet "Kura" $ newHashSet
  putStrLn "All elements in HashSet:"
  putStrLn . show . getElements $ hs
  putStrLn "Checking if HashSet contains element Kon"
  putStrLn . show $ containsElement "Kon" hs
  putStrLn "Checking if HashSet contains element Zyrafa"
  putStrLn . show $ containsElement "Zyrafa" hs
  putStrLn "Deleting Krowa from HashSet"
  hs' <- return . fst $ deleteElement "Krowa" hs
  putStrLn "All elements in HashSet after deletion"
  putStrLn . show . getElements $ hs'
