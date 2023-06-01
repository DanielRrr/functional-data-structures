module Chapter3.RedBlack where

data Colour = Red | Black
  deriving (Show, Eq)

data RBTree a =
    Leaf
  | RBNode Colour (RBTree a) a (RBTree a)
  deriving Show

member :: Eq a => a -> RBTree a -> Bool
member _ Leaf = False
member x (RBNode _ t1 y t2) =
  x == y || member x t1 || member x t2
