module Lib
    ( someFunc
    ) where

import AVLTree
import AVLSort

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
  putStrLn "1:"
  putStrLn $ show $ sortAVLAsc $ [39]
  putStrLn "2:"
  putStrLn $ show $ sortAVLAsc $ [26, 39]
  putStrLn "3:"
  putStrLn $ show $ sortAVLAsc $ [-3, 26, 39]
  putStrLn "4:"
  putStrLn $ show $ sortAVLAsc $ [99, -3, 26, 39]
  putStrLn "5:"
  putStrLn $ show $ sortAVLAsc $ [17, 99, -3, 26, 39]
  putStrLn "6:"
  putStrLn $ show $ sortAVLAsc $ [9, 17, 99, -3, 26, 39]
  putStrLn "7:"
  putStrLn $ show $ sortAVLAsc $ [6, 9, 17, 99, -3, 26, 39]
  putStrLn "8:"
  putStrLn $ show $ sortAVLAsc $ [7, 6, 9, 17, 99, -3, 26, 39]
  putStrLn "9:"
  putStrLn $ show $ sortAVLAsc $ [3, 7, 6, 9, 17, 99, -3, 26, 39]
