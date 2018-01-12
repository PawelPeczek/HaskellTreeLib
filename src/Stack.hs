module Stack(
          Stack(Stack),
          emptyStack,
          isEmpty,
          toList,
          push,
          pop
        ) where

newtype Stack a = Stack [a] deriving (Show)

emptyStack :: Stack a
emptyStack = Stack []

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty (Stack x) = False

toList :: Stack a -> [a]
toList (Stack x) = x

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> Maybe (Stack a, a)
pop (Stack []) = Nothing
pop (Stack (x:xs)) = Just (Stack xs, x)
