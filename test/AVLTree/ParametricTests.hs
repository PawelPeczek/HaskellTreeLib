{-# LANGUAGE TemplateHaskell #-}
module AVLTree.ParametricTests where

import Test.QuickCheck.All

prop_foo x y = x + y == y + x

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
