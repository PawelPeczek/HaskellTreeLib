module Lib
    ( someFunc
    ) where

import AVLTree


someFunc :: IO ()
someFunc = do
  putStrLn "Hellow World!"
  putStrLn $ show $ linearOrder $ insertKeyAsValue 7 .insertKeyAsValue 3 . insertKeyAsValue 7 . insertKeyAsValue 4 $ newTree
  putStrLn $ show $ depth $ insertKeyAsValue 7 .insertKeyAsValue 3 . insertKeyAsValue 7 . insertKeyAsValue 4 $ newTree
