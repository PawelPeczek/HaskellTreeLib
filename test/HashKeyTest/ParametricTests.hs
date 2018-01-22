{-# LANGUAGE TemplateHaskell #-}
module HashKeyTest.ParametricTests where

import Test.QuickCheck.All
import Test.QuickCheck
import Utils
import HashKey

prop_originalMatchesReturned x = x == (getOriginalKey . prepareKey $ x)

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
