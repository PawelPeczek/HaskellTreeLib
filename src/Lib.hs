module Lib
    ( someFunc
    ) where

import AVLTree


someFunc :: IO ()
someFunc = do
  putStrLn "Hellow World!"
  putStrLn $ show $ reversedOrder $ insert 3 .insert 3 . insert 7 . insert 8 $ newTree
