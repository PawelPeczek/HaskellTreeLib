--import AVLTree.UnitTests
{-# LANGUAGE TemplateHaskell #-}

import Test.HUnit
import Test.QuickCheck.All

import AVLTree
import AVLTreeTest.ParametricTests as TreeParametricTests
import AVLTreeTest.UnitTests as TreeUnitTests
import HashSetTest.ParametricTests as SetParametricTests
import HashSetTest.UnitTests as SetUnitTests


--main :: IO ()
main = do runTestTT TreeUnitTests.unitTests
          TreeParametricTests.runTests
          runTestTT SetUnitTests.unitTests
          SetParametricTests.runTests

