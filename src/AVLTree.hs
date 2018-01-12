{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : AVLTree
Description : Module with basic functionality of AVL Tree included
Copyright   : (c) Wojciech Geisler, 2018
                  Paweł Pęczek, 2018
-}
module AVLTree (
          newTree,
          linearOrder,
          reversedOrder,
          insert,
          debugShow
        ) where

import Stack

-- | Definiction of AVL tree.
data AVLTree a
  -- | AVLModule data constructor provides AVLNode of type a with the
  -- value of node element, parent left subtre, right subtree and balance coefficient
  = AVLNode a (AVLTree a) (AVLTree a) (AVLTree a) Int
  -- | EmptyTree data constructor is used to create NULL-node
  | EmptyNode

-- | Instantiation of Show class-type
instance Show a => Show (AVLTree a) where
  -- | The EmptyNode case - base case of structural recursion
  show EmptyNode = "(EmptyTree)"
  -- | The AVLNode case. Point that this function will recursively process whole
  -- AVL tree
  show (AVLNode x _ lt rt _) = "(" ++ show x ++ " " ++ show lt ++ ", " ++ show rt ++ ")"

-- | Debug function which provides a way to print additional info about balance
-- coefficients of nodes
debugShow :: Show a =>
  AVLTree a -- ^ 'AVLTree a' to be printed in debug mode
  -> String -- ^ String representation of tree
debugShow EmptyNode = "(EmptyTree)"
debugShow (AVLNode x _ lt rt bc) =
  "(" ++ show x ++ "[" ++ show bc ++ "] " ++ show lt ++ ", " ++ show rt ++ ")"

-- | Function returns AVLTree as an EmptyNode
newTree :: AVLTree a
newTree = EmptyNode

-- | Function returns list that consists of AVLTree elements in ascending order
linearOrder :: (Ord a) =>
  AVLTree a -- ^ 'AVLTree a' to linearize
  -> [a] -- ^ 'List' of elements in ascending order
linearOrder EmptyNode = []
linearOrder t = toList $ postorderWithStack t emptyStack

-- | Helper function that provides a way to make list of element in AVLTree
-- in O(n) time - as we always put element at the stack in O(1) instead
-- of simply concatenating lists at each of O(logN) levels in AVL Tree
-- Postorder -> to ensure correct order while popping from the "Stack" (FILO)
postorderWithStack :: (Ord a) =>
  AVLTree a -- ^ 'AVLTree a' to make operation on
  -> Stack a -- ^ 'Stack' to push elements at
  -> Stack a -- ^ 'Stack' - function output
postorderWithStack EmptyNode x = x
postorderWithStack (AVLNode n _ lt rt _) x =
  (postorderWithStack lt) $ (push n) $ (postorderWithStack rt x)

-- | Function returns list that consists of AVLTree elements in descending order
reversedOrder :: (Ord a) =>
  AVLTree a -- ^ 'AVLTree a' to linearize
  -> [a] -- ^ Output 'List'
reversedOrder = (reverse . linearOrder)

-- | Function that insert an element of type a into the AVLTree a.
-- This version of insert function provides a 'Set-like' insert
-- i.e. each element may appears only once at tree
insert :: (Ord a) =>
  a -- ^ Element to be inserted
  -> AVLTree a -- ^ 'AVLTree a' to which the element will be inserted
  -> AVLTree a -- ^ 'AVLTree a' after insertion of element
insert val EmptyNode = AVLNode val (EmptyNode) (EmptyNode) (EmptyNode) 0
insert val t = insertWithPartent val t EmptyNode where
  insertWithPartent val EmptyNode p = AVLNode val p (EmptyNode) (EmptyNode) 0
  insertWithPartent val (AVLNode x p lt rt bc) _ =
    if val > x then
      AVLNode x p lt (insertWithPartent val rt (AVLNode x p lt rt bc)) bc
    else if val < x then
      AVLNode x p (insertWithPartent val lt (AVLNode x p lt rt bc)) rt bc
    else AVLNode x p lt rt bc
