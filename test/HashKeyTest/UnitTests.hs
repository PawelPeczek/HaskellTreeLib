module HashKeyTest.UnitTests (
    unitTests
    ) where

import Test.HUnit
import HashKey 

test_prepareKeyHashesValue = TestCase $ do
    assertEqual "unexpected hash of integer" (HashKey (5, 5)) (prepareKey (5 :: Int))
    assertEqual "unexpectd hash of a string" (HashKey (7208697694940353632, "aa")) (prepareKey "aa")

unitTests = TestList  [test_prepareKeyHashesValue]



