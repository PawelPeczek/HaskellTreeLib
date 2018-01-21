{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : Lib
Description : Module with presentation of library possibilities
Copyright   : (c) Wojciech Geisler, 2018
                  Paweł Pęczek, 2018
-}
module Lib
    ( presentation
    ) where

import Examples
import AVLTree.Internal

-- | Function that presents possibilities of library
presentation :: IO () -- ^ function return IO () action
presentation = do
  putStrLn "HaskellTreeLib"
  putStrLn "Examples of AVLSort:"
  avlSortExample1
  avlSortExample2
  avlSortExample3
  putStrLn "Examples of HashMap"
  hashMapExample1
  putStrLn "Examples of HashSet"
  hashSetExample1
