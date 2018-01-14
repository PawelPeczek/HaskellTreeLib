--import AVLTree.UnitTests
{-# LANGUAGE TemplateHaskell #-}

import Test.HUnit
import Test.QuickCheck.All

import AVLTree
import AVLTreeTest.ParametricTests as ParametricTests
import AVLTreeTest.UnitTests as UnitTests


--main :: IO ()
main = do runTestTT UnitTests.unitTests
          ParametricTests.runTests

