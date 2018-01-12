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
          insertKeyAsValue,
          debugShow,
          depth
        ) where

import Stack

-- | Definition of balance coefficient in AVLTree
data BalanceCoeff
  -- | Both left and right subtree have the same size
  = Zero
  -- | LeftSubtree is one-level higher than right subtree
  | PlusOne
  -- | RightSubtree is one-level heigher than left subtree
  | MinusOne

-- | Function that maps BalanceCoeff into Int
numerizeBC ::
  BalanceCoeff -- ^ Input
  -> Int -- ^ Int value of input
numerizeBC Zero = 0
numerizeBC PlusOne = 1
numerizeBC MinusOne = -1

-- | Instantiation of Show class-type
instance Show BalanceCoeff where
  show Zero = "0"
  show PlusOne = "+1"
  show MinusOne = "-1"

-- | Definiction of AVL tree.
data AVLTree a b
  -- | AVLModule data constructor provides AVLNode of type a with the
  -- value of node key, value, parent left subtre, right subtree and balance coefficient
  = AVLNode a b (AVLTree a b) (AVLTree a b) BalanceCoeff
  -- | EmptyTree data constructor is used to create NULL-node
  | EmptyNode

-- | Instantiation of Show class-type
instance (Show a, Show b) => Show (AVLTree a b) where
  -- | The EmptyNode case - base case of structural recursion
  show EmptyNode = "(EmptyTree)"
  -- | The AVLNode case. Point that this function will recursively process whole
  -- AVL tree
  show (AVLNode x d lt rt _) = "(" ++ show x ++ " {data: " ++ show d ++ "} " ++ show lt ++ ", " ++ show rt ++ ")"

-- | Debug function which provides a way to print additional info about balance
-- coefficients of nodes
debugShow :: (Show a, Show b) =>
  AVLTree a b-- ^ 'AVLTree a b' to be printed in debug mode
  -> String -- ^ String representation of tree
debugShow EmptyNode = "(EmptyTree)"
debugShow (AVLNode x d lt rt bc) =
  "(" ++ show x ++ " [" ++ show bc ++ "] {data: " ++ show d ++ "}, " ++ debugShow lt ++ ", " ++ debugShow rt ++ ")"

-- | Function returns AVLTree as an EmptyNode
newTree :: AVLTree a b -- ^ Output: 'AVLTree a b' build with single EmptyNode
newTree = EmptyNode

-- | Function returns list that consists of AVLTree values in ascending order of keys
linearOrder :: (Ord a) =>
  AVLTree a b -- ^ 'AVLTree a b' to linearize
  -> [b] -- ^ 'List' of values in ascending order
linearOrder EmptyNode = []
linearOrder t = toList $ postorderWithStack t emptyStack

-- | Helper function that provides a way to make list of element in AVLTree
-- in O(n) time - as we always put element at the stack in O(1) instead
-- of simply concatenating lists at each of O(logN) levels in AVL Tree
-- Postorder -> to ensure correct order while popping from the "Stack" (FILO)
postorderWithStack :: (Ord a) =>
  AVLTree a b -- ^ 'AVLTree a b' to make operation on
  -> Stack b -- ^ 'Stack b' to push values at
  -> Stack b -- ^ 'Stack b' - function output
postorderWithStack EmptyNode x = x
postorderWithStack (AVLNode _ n lt rt _) x =
  (postorderWithStack lt) $ (push n) $ (postorderWithStack rt x)

-- | Function returns list that consists of AVLTree values in descending order of keys
reversedOrder :: (Ord a) =>
  AVLTree a b -- ^ 'AVLTree a b' to linearize
  -> [b] -- ^ Output 'List'
reversedOrder = (reverse . linearOrder)

-- | Function that insert an element key of type a and value of type b into the AVLTree a b.
-- This version of insert function provides a 'Set-like' insert
-- i.e. each element may appears only once at tree
-- Overall concept of insertion operation in functional language like Haskell
-- comes from https://gist.github.com/timjb/8292342 - but it's not simply a copy
-- of this code
insert :: (Ord a) =>
  a -- ^ Key of element to be inserted
  -> b -- ^ Value of the element to be inserted
  -> AVLTree a b -- ^ 'AVLTree a b' to which the element will be inserted
  -> AVLTree a b -- ^ 'AVLTree a b' after insertion of element
insert k v t = snd $ insert' k v t False

-- | Internal insert function that keep the AVLTree balanced (with heigh O(logN))
-- It returns pair (heightHasChanged, AVLTree) with
-- heightHasChanged - indicating change of height as a result of operation
-- AVLTree - tree after the operation
insert' :: (Ord a) =>
  a -- ^ Key (of type a) of element to insert
  -> b -- ^ Value (of type b) of element to insert
  -> AVLTree a b -- ^ 'AVLTree a b' to insert element to
  -> Bool -- ^ 'Bool' mode - True -> the same elements in tree allowed, False -> not alowed
  -> (Bool, AVLTree a b) -- ^ result pair described in details above
insert' k v EmptyNode _ = (True, AVLNode k v (EmptyNode) (EmptyNode) Zero)
insert' key val (AVLNode k v lt rt bc) mode =
  case (compare key k, mode) of
    (EQ, False) -> (False, AVLNode k v lt rt bc)
    (EQ, True) -> insertRight key val (AVLNode k v lt rt bc) mode
    (GT, _) -> insertRight key val (AVLNode k v lt rt bc) mode
    (LT, _) -> insertLeft key val (AVLNode k v lt rt bc) mode
  where
    insertRight key val (AVLNode k v lt rt bc) mode =
      let (heighChange, newSubTree) = insert' key val rt mode in
      case (heighChange, bc) of
        (False, _) -> (False, AVLNode k v lt newSubTree bc)
        (True, Zero) -> (True, AVLNode k v lt newSubTree MinusOne)
        (True, PlusOne) -> (True, AVLNode k v lt newSubTree Zero)
        (True, MinusOne) -> (False, rightRotation (AVLNode k v lt newSubTree Zero))
    insertLeft key val (AVLNode k v lt rt bc) mode =
      let (heighChange, newSubTree) = insert' key val lt mode in
      case (heighChange, bc) of
        (False, _) -> (False, AVLNode k v lt newSubTree bc)
        (True, Zero) -> (True, AVLNode k v newSubTree rt PlusOne)
        (True, MinusOne) -> (True, AVLNode k v newSubTree rt Zero)
        (True, PlusOne) -> (False, leftRotation (AVLNode k v newSubTree rt Zero))

-- Function performs right AVL rotationions
rightRotation ::
  AVLTree a b -- ^ (AVLTree a b) to operate on
  -> AVLTree a b -- ^ (AVLTree a b) after operation
rightRotation (AVLNode ak av alt (AVLNode bk bv blt brt bbc) abc) =
  case numerizeBC bbc of
    1 -> rlRotation (AVLNode ak av alt (AVLNode bk bv blt brt bbc) abc)
    otherwise -> rrRotation (AVLNode ak av alt (AVLNode bk bv blt brt bbc) abc)

-- Function performs left AVL rotationions
leftRotation ::
  AVLTree a b -- ^ (AVLTree a b) to operate on
  -> AVLTree a b -- ^ (AVLTree a b) after operation
leftRotation (AVLNode ak av (AVLNode bk bv blt brt bbc) art abc) =
  case numerizeBC bbc of
    -1 -> lrRotation (AVLNode ak av (AVLNode bk bv blt brt bbc) art abc)
    otherwise -> llRotation (AVLNode ak av (AVLNode bk bv blt brt bbc) art abc)

-- | Function that provides an implementation for RR AVLTree Rotations
-- see details: https://www.cise.ufl.edu/~nemo/cop3530/AVL-Tree-Rotations.pdf
rrRotation ::
  AVLTree a b -- ^ main AVL Tree node to rotate
  -> AVLTree a b -- ^ AVL Ttree node after rotation
rrRotation EmptyNode = EmptyNode
rrRotation (AVLNode ak av alt (AVLNode bk bv blt brt bbc) abc) =
  AVLNode bk bv (AVLNode ak av alt blt newABC) brt newBBC
  where
    newABC = if numerizeBC bbc == -1 then Zero else MinusOne
    newBBC = if numerizeBC bbc == -1 then Zero else PlusOne

-- | Function that provides an implementation for LL AVLTree Rotations
-- see details: https://www.cise.ufl.edu/~nemo/cop3530/AVL-Tree-Rotations.pdf
llRotation ::
  AVLTree a b -- ^ main AVL Tree node to rotate
  -> AVLTree a b -- ^ AVL Ttree node after rotation
llRotation EmptyNode = EmptyNode
llRotation (AVLNode ak av (AVLNode bk bv blt brt bbc) art abc) =
  AVLNode bk bv blt (AVLNode ak av brt art newABC) newBBC
  where
    newABC = if numerizeBC bbc == 1 then Zero else PlusOne
    newBBC = if numerizeBC bbc == 1 then Zero else MinusOne


-- | Function that provides an implementation for RL AVLTree Rotations
-- see details: https://www.cise.ufl.edu/~nemo/cop3530/AVL-Tree-Rotations.pdf
rlRotation ::
  AVLTree a b -- ^ main AVL Tree node to rotate
  -> AVLTree a b -- ^ AVL Ttree node after rotation
rlRotation EmptyNode = EmptyNode
rlRotation (AVLNode ak av alt (AVLNode bk bv (AVLNode ck cv clt crt cbc) brt bbc) abc) =
  AVLNode ck cv (AVLNode ak av alt clt newABC) (AVLNode bk bv crt brt newBBC) Zero
  where
    newABC = if numerizeBC cbc == -1 then PlusOne else Zero
    newBBC = if numerizeBC cbc == 1 then MinusOne else Zero

-- | Function that provides an implementation for RL AVLTree Rotations
-- see details: https://www.cise.ufl.edu/~nemo/cop3530/AVL-Tree-Rotations.pdf
lrRotation ::
  AVLTree a b -- ^ main AVL Tree node to rotate
  -> AVLTree a b -- ^ AVL Ttree node after rotation
lrRotation EmptyNode = EmptyNode
lrRotation (AVLNode ak av (AVLNode bk bv blt (AVLNode ck cv clt crt cbc) bbc) art abc) =
  AVLNode ck cv (AVLNode bk bv blt clt newBBC) (AVLNode ak av crt art newABC) Zero
  where
    newABC = if numerizeBC cbc == 1 then MinusOne else Zero
    newBBC = if numerizeBC cbc == -1 then PlusOne else Zero


-- | Function that performs delete operation at AVL Tree
-- returns pair (value, AVLTree) where value is the value associated with
-- deleted key and AVLTree is a tree after delete with height keeped O(logN)
-- if the key is not in a tree the function returns Just
delete :: Ord a =>
  a -- ^ key of type a to be deleted
  -> AVLTree a b -- ^ 'AVLTree a b' - the tree to operate on
  -> Maybe (b, AVLTree a b) -- ^ output as described above
delete k t =
  if containsKey k t then Just (v, newT)
  else Nothing
  where
    (v, newT, _) = delete' k t

-- | Function that determines whether a tree conatins given key or not
containsKey :: Ord a =>
  a -- ^ Key to be found of type a
  -> AVLTree a b -- ^ 'AVLTree a b' to find key in
  -> Bool -- ^ result - indicates the success status of search
containsKey _ EmptyNode = False
containsKey key (AVLNode k _ lt rt _) =
  if key == k then True
  else (containsKey key lt) || (containsKey key rt)

-- | Function performs actual deletion from AVLTree.
-- It returns (value, AVLTree, heightChanged) deleted value, AVLTree after
-- operation and height changing indicator (heightChanged)
delete' :: Ord a =>
    a -- ^ Key to be found of type a
    -> AVLTree a b -- ^ 'AVLTree a b' to find key in
    -> (b, AVLTree a b, Bool) -- ^ output as described above
delete' key (AVLNode k v lt rt bc) =
  case compare key k of
    EQ -> error "not implemented"
    GT -> deleteRight key (AVLNode k v lt rt bc)
    LT -> deleteLeft key (AVLNode k v lt rt bc)
  where
    deleteRight key (AVLNode k v lt rt bc) = error "not implemented"
    deleteLeft key (AVLNode k v lt rt bc) = error "not implemented"


-- | Function inserts the same value as key and value of AVLNode using
-- for that insert function (see above).
insertKeyAsValue :: (Ord a) =>
  a -- ^ Key and value of element to be inserted
  -> AVLTree a a -- ^ 'AVLTree a b' to which the element will be inserted
  -> AVLTree a a -- ^ 'AVLTree a b' after insertion of element
insertKeyAsValue x = insert x x

-- | Function that returns depth of tree
depth ::
  AVLTree a b -- ^ 'AVLTree a b' to measure depth
  -> Int -- ^ 'Int' depth of tree (with regards that depth of EmptyNode = 0)
depth EmptyNode = 0
depth (AVLNode _ _ lt rt _) = max (depth lt) (depth rt) + 1
