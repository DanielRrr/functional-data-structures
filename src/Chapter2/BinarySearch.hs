module Chapter2.BinarySearch where

import Data.Maybe (fromMaybe)

data Tree a = E | T (Tree a) a (Tree a)
  deriving (Show, Eq)

empty :: Tree a
empty = E

member :: Ord a => a -> Tree a -> Bool
member _ E = False
member x (T l y r) =
  case x < y of
    True -> member x l
    False -> if y < x then member x r else True

insert :: Ord a => a -> Tree a -> Tree a
insert x E = T E x E
insert x t@(T l y r) =
  case x < y of
    True -> T (insert x l) y r
    False -> 
        if y < x 
        then T l y (insert x r)
        else t

member' :: Ord a => a -> Tree a -> Bool
member' _ E = False
member' x (T l y r) =
  x == y || if x < y then member' x l else member' x r

insert' :: Ord a => a -> Tree a -> Tree a
insert' x E = T E x E
insert' x t = fromMaybe t (insert'' x t)
  where
    insert'' x' E = Just (T E x' E)
    insert'' x' (T l y r)
      | x' < y = (\left -> T left y r) <$> insert'' x' l
      | x' > y = T l y <$> insert'' x' r
      | otherwise = Nothing

complete :: Int -> a -> Tree a
complete 0 _ = E
complete n x =
  let com = complete (n - 1) x in
    T com x com

balanced :: Int -> a -> Tree a
balanced 0 _ = E
balanced n x = T (balanced n' x) x (balanced n'' x)
  where
    n' = quot (n - 1) 2
    n'' = n - 1 - n'