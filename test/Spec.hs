--import AVLTree.UnitTests
{-# LANGUAGE TemplateHaskell #-}

import Test.HUnit
import Test.QuickCheck.All

import AVLTree
import AVLTree.ParametricTests as ParametricTests

--main :: IO ()
main = do --runTestTT tests
          ParametricTests.runTests

