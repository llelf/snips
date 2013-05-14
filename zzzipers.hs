{-# LANGUAGE DeriveDataTypeable #-}

import Data.Generics.Zipper
import Data.Data


-- | our tree def
data Tree a = Empty | Fork a (Tree a) (Tree a)
              deriving (Typeable,Data)

-- | how we print it
instance Show a => Show (Tree a) where
    show Empty = ""
    show (Fork x l r) = show x ++ sub "L" l ++ sub "R" r
        where sub _ Empty = ""
              sub s t     = "-" ++ s ++ "(" ++ show t ++ ")"


-- example
t :: Tree Int
t = Fork 0 (Fork 1 Empty Empty) (Fork 2 (Fork 3 Empty Empty) (Fork 4 Empty Empty))


-- zipper (by magic)
z = toZipper t


pp :: Maybe (Zipper (Tree Int)) -> Maybe (Tree Int)
pp (Just z) = getHole z


{- In interpreter:

H> pp $ Just z
Just 0-L(1)-R(2-L(3)-R(4))

H> pp $ Just z >>= down
Just 2-L(3)-R(4)

H> pp $ Just z >>= down >>= left
Just 1

H> pp $ Just z >>= down >>= left >>= right
Just 2-L(3)-R(4)

H> pp $ Just z >>= down >>= left >>= right >>= down
Just 4

H> pp $ Just z >>= down >>= left >>= right >>= down >>= left
Just 3

H> pp $ Just z >>= down >>= left >>= right >>= down >>= left >>= up
Just 2-L(3)-R(4)

H> pp $ Just z >>= down >>= left >>= right >>= down >>= left >>= up >>= left
Just 1

-}



