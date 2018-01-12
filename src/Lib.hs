module Lib
    ( someFunc
    ) where

import AVLTree


someFunc :: IO ()
someFunc = do
  putStrLn "Hellow World!"
  putStrLn $ show $ linearOrder $ insertKeyAsValue 7 . insertKeyAsValue 6 . insertKeyAsValue 5 . insertKeyAsValue 4 . insertKeyAsValue 3 . insertKeyAsValue 2 . insertKeyAsValue 1 $ newTree
  putStrLn $ show $ depth $ insertKeyAsValue 7 . insertKeyAsValue 6 . insertKeyAsValue 5 . insertKeyAsValue 4 . insertKeyAsValue 3 . insertKeyAsValue 2 . insertKeyAsValue 1 $ newTree
  putStrLn $ debugShow $ insertKeyAsValue 7 . insertKeyAsValue 6 . insertKeyAsValue 5 . insertKeyAsValue 4 . insertKeyAsValue 3 . insertKeyAsValue 2 . insertKeyAsValue 1 $ newTree
  -- putStrLn $ show $ insertKeyAsValue 11 . insertKeyAsValue 10 . insertKeyAsValue 9 . insertKeyAsValue 7 . insertKeyAsValue 3 . insertKeyAsValue 7 . insertKeyAsValue 4 $ newTree
