{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : AVLTree
Description : Module with basic functionality of AVL Tree included
Copyright   : (c) Wojciech Geisler, 2018
                  Paweł Pęczek, 2018
-}
module AVLTree (
          AVLTree,
          newTree,
          linearOrder,
          reversedOrder,
          linearKeysOrder,
          reversedKeysOrder,
          insert,
          insertSingleton,
          insertKeyAsValue,
          insertKeyAsValueSingleton,
          debugShow,
          depth,
          delete,
          containsKey,
          delete,
          getValueOfKey,
          (&:)
        ) where

import Stack

-- | Definition of data type used as value if we want to identify key and
-- value while inserting
data NULL = NULL deriving(Show)

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

-- | Function that returns list of keys in AVLTree in ascending order
linearKeysOrder :: (Ord a) =>
  AVLTree a b -- ^ 'AVLTree a b' to linearize
  -> [a] -- ^ 'List' of keys in ascending order
linearKeysOrder EmptyNode = []
linearKeysOrder t = toList $ postorderKeysWithStack t emptyStack

-- | Helper function that provides a way to make list of keys in AVLTree
-- in O(n) time - as we always put element at the stack in O(1) instead
-- of simply concatenating lists at each of O(logN) levels in AVL Tree
-- Postorder -> to ensure correct order while popping from the "Stack" (FILO)
postorderKeysWithStack :: (Ord a) =>
  AVLTree a b -- ^ 'AVLTree a b' to make operation on
  -> Stack a -- ^ 'Stack b' to push values at
  -> Stack a -- ^ 'Stack b' - function output
postorderKeysWithStack EmptyNode x = x
postorderKeysWithStack (AVLNode n _ lt rt _) x =
  (postorderKeysWithStack lt) $ (push n) $ (postorderKeysWithStack rt x)

-- | Function returns list that consists of AVLTree keys in descending order of keys
reversedKeysOrder :: (Ord a) =>
  AVLTree a b -- ^ 'AVLTree a b' to linearize
  -> [a] -- ^ Output 'List'
reversedKeysOrder = (reverse . linearKeysOrder)

-- | Function that insert an element key of type a and value of type b into the AVLTree a b.
-- This version of insert function provides a straightforward insert
-- i.e. each element may appears multiple times at the tree.
insert :: (Ord a) =>
  a -- ^ Key of element to be inserted
  -> b -- ^ Value of the element to be inserted
  -> AVLTree a b -- ^ 'AVLTree a b' to which the element will be inserted
  -> AVLTree a b -- ^ 'AVLTree a b' after insertion of element
insert k v t = snd $ insert' k v t True

-- | Function that insert an element key of type a and value of type b into the AVLTree a b.
-- This version of insert function provides a 'Set-like' insert
-- i.e. each element may appears only once at the tree.
-- Overall concept of insertion operation in functional language like Haskell
-- comes from https://gist.github.com/timjb/8292342 - but it's not simply a copy
-- of this code
insertSingleton :: (Ord a) =>
  a -- ^ Key of element to be inserted
  -> b -- ^ Value of the element to be inserted
  -> AVLTree a b -- ^ 'AVLTree a b' to which the element will be inserted
  -> AVLTree a b -- ^ 'AVLTree a b' after insertion of element
insertSingleton k v t = snd $ insert' k v t False

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
    (EQ, False) -> (False, AVLNode k val lt rt bc)
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
        (False, _) -> (False, AVLNode k v newSubTree rt bc)
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
  -> (AVLTree a b, Maybe b) -- ^ output as described above
delete k t =
  if containsKey k t then (newT, Just v)
  else (t, Nothing)
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

-- | Function gets BalanceCoeff from given AVLTree
getBC ::
  AVLTree a b -- ^ 'AVLTree a b' to find BalanceCoeff
  -> BalanceCoeff -- ^ result as described above
getBC EmptyNode = error "Unsupported operation!"
getBC (AVLNode _ _ _ _ bc) = bc

-- | Function performs actual deletion from AVLTree.
-- It returns (value, AVLTree, heightChanged) deleted value, AVLTree after
-- operation and height changing indicator (heightChanged)
delete' :: Ord a =>
    a -- ^ Key to be found of type a
    -> AVLTree a b -- ^ 'AVLTree a b' to find key in
    -> (b, AVLTree a b, Bool) -- ^ output as described above
delete' key (AVLNode k v lt rt bc) =
  case compare key k of
    EQ -> actualDelete (AVLNode k v lt rt bc)
    GT -> deleteRight key (AVLNode k v lt rt bc)
    LT -> deleteLeft key (AVLNode k v lt rt bc)
  where
    deleteRight key (AVLNode k v lt rt bc) =
      let (delV, rt', heighChg) = delete' key rt in
      case (heighChg, bc, getBC rt') of
        (False, _, _) -> (delV, (AVLNode k v lt rt' bc), False)
        (True, Zero, _) -> (delV, (AVLNode k v lt rt' PlusOne), False)
        (True, MinusOne, _) -> (delV, (AVLNode k v lt rt' Zero), True)
        (True, PlusOne, Zero) -> (delV, llRotation (AVLNode k v lt rt' Zero), False)
        (True, PlusOne, PlusOne) -> (delV, llRotation (AVLNode k v lt rt' Zero), True)
        (True, PlusOne, MinusOne) -> (delV, lrRotation (AVLNode k v lt rt' Zero), True)
    deleteLeft key (AVLNode k v lt rt bc) =
      let (delV, lt', heighChg) = delete' key lt in
      case (heighChg, bc, getBC lt') of
        (False, _, _) -> (delV, (AVLNode k v lt' rt bc), False)
        (True, Zero, _) -> (delV, (AVLNode k v lt' rt MinusOne), False)
        (True, PlusOne, _) -> (delV, (AVLNode k v lt' rt Zero), True)
        (True, MinusOne, Zero) -> (delV, rrRotation (AVLNode k v lt' rt Zero), False)
        (True, MinusOne, MinusOne) -> (delV, rrRotation (AVLNode k v lt' rt Zero), True)
        (True, MinusOne, PlusOne) -> (delV, rlRotation (AVLNode k v lt' rt Zero), True)

-- | Function that performs actuall deletion of tree node
-- to match the convention of delete' it returns
-- (value, AVLTree, heightChanged) deleted value, AVLTree after
-- operation and height changing indicator (heightChanged)
actualDelete :: (Ord a) =>
  AVLTree a b -- ^ 'AVLTree a b' to delete (first node will be deleted)
  -> (b, AVLTree a b, Bool) -- ^ output as described above
actualDelete (AVLNode k v (AVLNode lk lv llt lrt lbc) (AVLNode rk rv rlt rrt rbc) bc) =
  let (v', t', heighChg) = delete' (getKey predecessor) (AVLNode lk lv llt lrt lbc) in
  case  (heighChg, bc) of
    (False, _) -> (v, AVLNode (getKey predecessor) v' t' (AVLNode rk rv rlt rrt rbc) bc, False)
    (True, Zero) -> (v, AVLNode (getKey predecessor) v' t' (AVLNode rk rv rlt rrt rbc) PlusOne, False)
    (True, PlusOne) -> (v, AVLNode (getKey predecessor) v' t' (AVLNode rk rv rlt rrt rbc) Zero, False)
    -- rotation case -> now old node rt has bc +2 -> some right rotation
    (True, MinusOne) -> (v, rightRotation (AVLNode (getKey predecessor) v' t' (AVLNode rk rv rlt rrt rbc) Zero), True)
  where
    predecessor = getMaxElem (AVLNode lk lv llt lrt lbc)
actualDelete (AVLNode k v EmptyNode (AVLNode rk rv rlt rrt rbc) bc) =
  (v, (AVLNode rk rv rlt rrt rbc), True)
actualDelete (AVLNode k v (AVLNode lk lv llt lrt lbc) EmptyNode bc) =
  (v, (AVLNode lk lv llt lrt lbc), True)
actualDelete (AVLNode k v EmptyNode EmptyNode bc) = (v, EmptyNode, True)

-- | Function returns maximum element in a tree
getMaxElem ::
  AVLTree a b -- ^ tree to find maximum
  -> AVLTree a b -- ^ maximum element in tree
getMaxElem (AVLNode k v lt EmptyNode bc) = (AVLNode k v lt EmptyNode bc)
getMaxElem (AVLNode k v lt rt bc) = getMaxElem rt

-- | Function that returns first key from given AVLTree (bascially allowed only for
-- AVLNode data constructor)
getKey ::
  AVLTree a b -- ^ 'AVLTree a b' to get the first element key
  -> a -- ^ requested key
getKey EmptyNode = error "Unsupported operation!"
getKey (AVLNode k _ _ _ _) = k

-- | Function that returns first value from given AVLTree (bascially allowed only for
-- AVLNode data constructor)
getValue ::
  AVLTree a b -- ^ 'AVLTree a b' to get the first element value
  -> b -- ^ requested value
getValue EmptyNode = error "Unsupported operation!"
getValue (AVLNode _ v _ _ _) = v

-- | Function inserts the same value as key and value of AVLNode using
-- for that insert function (see above). Repetition of keys in tree - allowed.
insertKeyAsValue :: (Ord a) =>
  a -- ^ Key and value of element to be inserted
  -> AVLTree a () -- ^ 'AVLTree a ()' to which the element will be inserted
  -> AVLTree a () -- ^ 'AVLTree a ()' after insertion of element
insertKeyAsValue x = insert x ()

-- | Function inserts the same value as key and value of AVLNode using
-- for that insert function (see above). Repetition of keys in tree - forbidden.
insertKeyAsValueSingleton :: (Ord a) =>
  a -- ^ Key and value of element to be inserted
  -> AVLTree a () -- ^ 'AVLTree a ()' to which the element will be inserted
  -> AVLTree a () -- ^ 'AVLTree a ()' after insertion of element
insertKeyAsValueSingleton x = insertSingleton x ()

-- | Function that returns depth of tree
depth ::
  AVLTree a b -- ^ 'AVLTree a b' to measure depth
  -> Int -- ^ 'Int' depth of tree (with regards that depth of EmptyNode = 0)
depth EmptyNode = 0
depth (AVLNode _ _ lt rt _) = max (depth lt) (depth rt) + 1

-- | Function that get element with a given key from given AVLTree
-- without deleting it. The function returns (AVLTree, Maybe value)
-- so that in case of success the requested value is placed in Just value,
-- otherwise the second element of pair is Nothing
getValueOfKey :: (Ord a) =>
  a -- ^ searching ket of type a
  -> AVLTree a b -- ^ 'AVLTree a b' to look for element
  -> (AVLTree a b, Maybe b) -- ^ result as described above
getValueOfKey k t =
  if containsKey k t == True then (t, Just (getFromTree k t))
  else (t, Nothing)
  where
    getFromTree _ EmptyNode = error "Unsupported operation!"
    getFromTree key (AVLNode k v lt rt _) =
      if key == k then v
      else if key > k then getFromTree key rt
      else getFromTree key lt

-- | Infix operator for adding a key value pair to a tree
(&:) :: Ord a => (a, b) -> AVLTree a b -> AVLTree a b
(&:) (k, v) t = insert k v t
