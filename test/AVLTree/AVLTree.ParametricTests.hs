{-# LANGUAGE TemplateHaskell #-}
module AVLTree.ParametricTests where

import Test.QuickCheck.All

prop_foo x y = x + y == y + x

--prop_bar = ...

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
