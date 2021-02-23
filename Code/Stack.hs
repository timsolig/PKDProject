{- Implements a stack (last in first out). -}
module Stack (Stack(), empty, isEmpty, push, top, pop) where

-- interface
{- empty ... -}
empty :: Stack a
{- isEmpty s ... -}
isEmpty :: Stack a -> Bool
{- ... -}
push :: a -> Stack a -> Stack a
{- ... -}
top :: Stack a -> a
{- ... -}
pop :: Stack a -> (a,Stack a)


-- implementation
{- A stack (Last In First Out): the head of the list is
    the top of the stack, the 2nd element of the list is
    the element below the top, and so on.
-}
newtype Stack a = StackImpl [a] -- opaque!

empty = StackImpl []
isEmpty (StackImpl s) = null s
push x (StackImpl s) = StackImpl (x:s)
top (StackImpl s) = head s
pop (StackImpl (s:ss)) = (s,StackImpl ss)
