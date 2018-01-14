module AVLTreeTest.UnitTests (
    unitTests
    ) where

import Test.HUnit

import AVLTree
import AVLTree.Internal

validTree = 
        (AVLNode 0 0
            (AVLNode (-5) (-5)
                (AVLNode (-10) (-10) EmptyNode EmptyNode Zero)
                (AVLNode (-3) (-3) EmptyNode EmptyNode Zero)
            Zero)
            (AVLNode 5 5
                (AVLNode 3 3 EmptyNode EmptyNode Zero)
                (AVLNode 10 10 EmptyNode EmptyNode Zero)
            Zero)
        Zero)

test_isValidDetectsInvalidTree = TestCase (do
    t <- return (AVLNode 2 1 (AVLNode 1 0 (AVLNode 0 0 EmptyNode EmptyNode Zero) EmptyNode MinusOne) EmptyNode Zero)
    assertEqual "Invalid tree not detected"  False (isValid  t)
    )

test_isValidDetectsValidTree = TestCase $ do
    t <- return validTree
    assertEqual "Valid tree should pass isValid" True (isValid  t)


validityTests = TestLabel "Tests of validity checks" (TestList [
        test_isValidDetectsInvalidTree,
        test_isValidDetectsValidTree
    ])

test_llRotation = TestCase $ do
    expected <- return $ 
        (AVLNode (-5) (-5)
            (AVLNode (-10) (-10)
                EmptyNode
                EmptyNode
            Zero)
            (AVLNode 0 0
                (AVLNode (-3) (-3) EmptyNode EmptyNode Zero)
                (AVLNode 5 5 
                    (AVLNode 3 3 EmptyNode EmptyNode Zero)
                    (AVLNode 10 10 EmptyNode EmptyNode Zero)
                Zero)
            MinusOne)
        MinusOne)
    assertEqual "LL rotation failed" expected (llRotation validTree)

rotationTests = TestLabel "Rotation testss" (TestList [
        test_llRotation
    ])


unitTests = TestList [validityTests, rotationTests]

--main :: IO Counts
--main = do _ <- runTestTT tests
--               runTestTT tests'
