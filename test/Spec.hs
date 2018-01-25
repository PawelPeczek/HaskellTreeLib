--import AVLTree.UnitTests
{-# LANGUAGE TemplateHaskell #-}

import Test.HUnit
import Test.QuickCheck.All

import AVLTree
import AVLTreeTest.ParametricTests as TreeParametricTests
import AVLTreeTest.UnitTests as TreeUnitTests
import HashKeyTest.UnitTests as HashKeyUnitTests
import HashKeyTest.ParametricTests as HashKeyParametricTests
import HashSetTest.ParametricTests as SetParametricTests
import HashMapTest.ParametricTests as MapParametricTests


--main :: IO ()
main = do runTestTT TreeUnitTests.unitTests
          TreeParametricTests.runTests
          SetParametricTests.runTests
          MapParametricTests.runTests
          runTestTT HashKeyUnitTests.unitTests
          HashKeyParametricTests.runTests


