module Lib
    ( someFunc
    ) where

import AVLTree
import AVLSort
import HashMap
import HashSet

someFunc :: IO ()
someFunc = do
  putStrLn "Hellow World!"
  -- putStrLn $ show $ linearOrder $ insertKeyAsValue 7 . insertKeyAsValue 6 . insertKeyAsValue 5 . insertKeyAsValue 4 . insertKeyAsValue 3 . insertKeyAsValue 2 . insertKeyAsValue 1 $ newTree
  -- putStrLn $ show $ depth $ insertKeyAsValue 7 . insertKeyAsValue 6 . insertKeyAsValue 5 . insertKeyAsValue 4 . insertKeyAsValue 3 . insertKeyAsValue 2 . insertKeyAsValue 1 $ newTree
  -- putStrLn $ debugShow $ insertKeyAsValue 7 . insertKeyAsValue 6 . insertKeyAsValue 5 . insertKeyAsValue 4 . insertKeyAsValue 3 . insertKeyAsValue 2 . insertKeyAsValue 1 $ newTree
  -- putStrLn $ debugShow $ snd $ delete 5 . insertKeyAsValue 7 . insertKeyAsValue 6 . insertKeyAsValue 5 . insertKeyAsValue 4 . insertKeyAsValue 3 . insertKeyAsValue 2 . insertKeyAsValue 1 $ newTree
  -- putStrLn $ show $ linearOrder $ snd $ delete 5 . insertKeyAsValue 7 . insertKeyAsValue 6 . insertKeyAsValue 5 . insertKeyAsValue 4 . insertKeyAsValue 3 . insertKeyAsValue 2 . insertKeyAsValue 1 $ newTree
  -- putStrLn $ show $ depth $ snd $ delete 5 . insertKeyAsValue 7 . insertKeyAsValue 6 . insertKeyAsValue 5 . insertKeyAsValue 4 . insertKeyAsValue 3 . insertKeyAsValue 2 . insertKeyAsValue 1 $ newTree
  -- putStrLn $ debugShow $ snd $ delete 7 . snd $ delete 5 . insertKeyAsValue 7 . insertKeyAsValue 6 . insertKeyAsValue 5 . insertKeyAsValue 4 . insertKeyAsValue 3 . insertKeyAsValue 2 . insertKeyAsValue 1 $ newTree
  -- putStrLn $ show $ linearOrder $ snd $ delete 7 . snd $ delete 5 . insertKeyAsValue 7 . insertKeyAsValue 6 . insertKeyAsValue 5 . insertKeyAsValue 4 . insertKeyAsValue 3 . insertKeyAsValue 2 . insertKeyAsValue 1 $ newTree
  -- putStrLn $ show $ depth $ snd $ delete 7 . snd $ delete 5 . insertKeyAsValue 7 . insertKeyAsValue 6 . insertKeyAsValue 5 . insertKeyAsValue 4 . insertKeyAsValue 3 . insertKeyAsValue 2 . insertKeyAsValue 1 $ newTree
  -- putStrLn $ show $ insertKeyAsValue 11 . insertKeyAsValue 10 . insertKeyAsValue 9 . insertKeyAsValue 7 . insertKeyAsValue 3 . insertKeyAsValue 7 . insertKeyAsValue 4 $ newTree
  -- putStrLn "1:"
  -- putStrLn $ show $ sortAVLAsc $ [39]
  -- putStrLn "2:"
  -- putStrLn $ show $ sortAVLAsc $ [26, 39]
  -- putStrLn "3:"
  -- putStrLn $ show $ sortAVLAsc $ [-3, 26, 39]
  -- putStrLn "4:"
  -- putStrLn $ show $ sortAVLAsc $ [99, -3, 26, 39]
  -- putStrLn "5:"
  -- putStrLn $ show $ sortAVLAsc $ [17, 99, -3, 26, 39]
  -- putStrLn "6:"
  -- putStrLn $ show $ sortAVLAsc $ [9, 17, 99, -3, 26, 39]
  -- putStrLn "7:"
  -- putStrLn $ show $ sortAVLAsc $ [6, 9, 17, 99, -3, 26, 39]
  -- putStrLn "8:"
  -- putStrLn $ show $ sortAVLAsc $ [7, 6, 9, 17, 99, -3, 26, 39]
  -- putStrLn "9:"
  -- putStrLn $ show $ sortAVLAsc $ [3, 7, 6, 9, 17, 99, -3, 26, 39]
  -- putStrLn $ show $ getKeys . insertKeyVal "Sławek" 188  . insertKeyVal "Wojtek" 252  . insertKeyVal "Jan" 388 . insertKeyVal "Diminik" 38 . insertKeyVal "Ksawery" 88 . insertKeyVal "Jan" 19 $ newHashMap
  -- putStrLn $ show $ insertKeyVal "Sławek" 188  . insertKeyVal "Wojtek" 252  . insertKeyVal "Jan" 388 . insertKeyVal "Diminik" 38 . insertKeyVal "Ksawery" 88 . insertKeyVal "Jan" 19 $ newHashMap
  -- putStrLn $ show $ getValues . fst $ deleteFromHM "Jan" . insertKeyVal "Sławek" 188  . insertKeyVal "Wojtek" 252  . insertKeyVal "Jan" 388 . insertKeyVal "Diminik" 38 . insertKeyVal "Ksawery" 88 . insertKeyVal "Jan" 19 $ newHashMap
  -- putStrLn $ show $ fst $ deleteFromHM "Jan" . insertKeyVal "Sławek" 188  . insertKeyVal "Wojtek" 252  . insertKeyVal "Jan" 388 . insertKeyVal "Diminik" 38 . insertKeyVal "Ksawery" 88 . insertKeyVal "Jan" 19 $ newHashMap
  -- putStrLn $ show $ HashMap.containsKey "Jan" . fst $ deleteFromHM "Jan" . insertKeyVal "Sławek" 188  . insertKeyVal "Wojtek" 252  . insertKeyVal "Jan" 388 . insertKeyVal "Diminik" 38 . insertKeyVal "Ksawery" 88 . insertKeyVal "Jan" 19 $ newHashMap
  -- putStrLn "1:"
  -- putStrLn $ debugShow $ AVLTree.fromList [(0,0)]
  -- putStrLn "2:"
  -- putStrLn $ debugShow $ AVLTree.fromList [(1,0),(0,0)]
  -- putStrLn "3:"
  -- putStrLn $ debugShow $ AVLTree.fromList [(0,0),(1,0),(0,0)]
  -- putStrLn "4:"
  -- putStrLn $ debugShow $ AVLTree.fromList [(0,0),(0,0),(1,0),(0,0)]
  -- putStrLn "5:"
  -- putStrLn $ debugShow $ AVLTree.fromList [(1,0),(0,0),(0,0),(1,0),(0,0)]
  -- putStrLn "6:"
  -- putStrLn $ debugShow $ AVLTree.fromList [(0,0),(1,0),(0,0),(0,0),(1,0),(0,0)]
  -- putStrLn $ debugShow $ AVLTree.fromList [(-1,0),(-1,0),(0,0),(-1,0),(0,0),(-1,0)]
  putStrLn $ debugShow $ AVLTree.fromList [(0,0),(1,0),(-1,0),(0,0),(0,0),(0,0),(1,0)]
  putStrLn "After del 0:"
  putStrLn $ debugShow $ fst . AVLTree.delete (0) $ AVLTree.fromList [(0,0),(1,0),(-1,0),(0,0),(0,0),(0,0),(1,0)]
  putStrLn "After del 1:"
  putStrLn $ debugShow $ fst . AVLTree.delete (1) $ AVLTree.fromList [(0,0),(1,0),(-1,0),(0,0),(0,0),(0,0),(1,0)]
  putStrLn "After del -1:"
  putStrLn $ debugShow $ fst . AVLTree.delete (-1) $ AVLTree.fromList [(0,0),(1,0),(-1,0),(0,0),(0,0),(0,0),(1,0)]
