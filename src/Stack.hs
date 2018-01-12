{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : AVLTree
Description : Module with basic functionality of Stack data structure
Copyright   : (c) Wojciech Geisler, 2018
                  Paweł Pęczek, 2018
-}
module Stack(
          Stack(Stack),
          emptyStack,
          isEmpty,
          toList,
          push,
          pop
        ) where

-- | Definition of Stack
newtype Stack a = Stack [a] deriving (Show)

-- | Function that returns empty stack
emptyStack :: Stack a -- ^ Function return (empty 'Stack')
emptyStack = Stack []

-- | Function that cheks whether tge given Stack is empty
isEmpty ::
  Stack a -- ^ Input 'Stack a'
  -> Bool -- ^ Boolean value indicates emptiness of input 'Stack a'
isEmpty (Stack []) = True
isEmpty (Stack x) = False

-- | Function that produces list from given Stack in reversed insertion order
toList ::
  Stack a -- ^ Input 'Stack a'
  -> [a] -- ^ Output: list of elements from the stack in LIFO order
toList (Stack x) = x

-- | Function that pushes element at the top of stack
push ::
  a -- ^ Element of type a to be pushed on stack
  -> Stack a -- ^ 'Stack a' to push the element
  -> Stack a -- ^ 'Stack a' after the operation
push x (Stack xs) = Stack (x:xs)

-- | Function that pops the top alement from stack returns this element in pair
-- with the stack after the operation. If the stack is initially empty - the
-- function returns "Nothing"
pop ::
  Stack a -- ^ 'Stack a' to be popped from
  -> Maybe (Stack a, a) -- Return packaged in Maybe context
pop (Stack []) = Nothing
pop (Stack (x:xs)) = Just (Stack xs, x)
