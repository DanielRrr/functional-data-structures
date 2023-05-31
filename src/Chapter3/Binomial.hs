module Chapter3.Binomial where

data Tree a = Node Int a (Tree [a])
  deriving Show

type Heap a = [Tree a]