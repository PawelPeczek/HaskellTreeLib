--import AVLTree.UnitTests
{-# LANGUAGE TemplateHaskell #-}

import Test.HUnit
import Test.QuickCheck.All

import AVLTree
import AVLTreeTest.ParametricTests as TreeParametricTests
import AVLTreeTest.UnitTests as TreeUnitTests
import HashSetTest.ParametricTests as SetParametricTests
import HashKeyTest.UnitTests as HashKeyUnitTests
import HashKeyTest.ParametricTests as HashKeyParametricTests


--main :: IO ()
main = do runTestTT TreeUnitTests.unitTests
          TreeParametricTests.runTests
          SetParametricTests.runTests
          runTestTT HashKeyUnitTests.unitTests
          HashKeyParametricTests.runTests


