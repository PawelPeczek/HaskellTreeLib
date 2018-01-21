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
    let input = (AVLNode 1 1 EmptyNode (AVLNode 2 2 EmptyNode (AVLNode 3 3 EmptyNode EmptyNode Zero) MinusOne) MinusOne)
    let expected = (AVLNode 2 2 (AVLNode 1 1 EmptyNode EmptyNode Zero) (AVLNode 3 3 EmptyNode EmptyNode Zero) Zero)
    assertEqual "LL rotation failed" expected (llRotation input)

test_rrRotation = TestCase $ do
    let input = (AVLNode 1 1 (AVLNode 2 2 (AVLNode 3 3 EmptyNode EmptyNode Zero) EmptyNode PlusOne) EmptyNode PlusOne)
    let expected = (AVLNode 2 2 (AVLNode 3 3 EmptyNode EmptyNode Zero) (AVLNode 1 1 EmptyNode EmptyNode Zero) Zero)
    assertEqual "RR rotation failed" expected (rrRotation input)

test_lrRotation = TestCase $ do
    let input = (AVLNode 1 1 EmptyNode (AVLNode 3 3 (AVLNode 2 2 EmptyNode EmptyNode Zero) EmptyNode PlusOne) MinusOne)
    let expected = (AVLNode 2 2 (AVLNode 1 1 EmptyNode EmptyNode Zero) (AVLNode 3 3 EmptyNode EmptyNode Zero) Zero)
    let result= lrRotation input
    assertEqual "LR rotation failed" expected result


test_rlRotation = TestCase $ do
    let input = (AVLNode 3 3 (AVLNode 1 1 EmptyNode (AVLNode 2 2 EmptyNode EmptyNode Zero) PlusOne) EmptyNode PlusOne)
    let expected = (AVLNode 2 2 (AVLNode 1 1 EmptyNode EmptyNode Zero) (AVLNode 3 3 EmptyNode EmptyNode Zero) Zero)
    let result= rlRotation input
    assertEqual "RL rotation failed" expected result


rotationTests = TestLabel "Rotation testss" (TestList [
        test_llRotation,
        test_rrRotation,
        test_lrRotation,
        test_rlRotation
    ])


unitTests = TestList [validityTests, rotationTests]

--main :: IO Counts
--main = do _ <- runTestTT tests
--               runTestTT tests'
