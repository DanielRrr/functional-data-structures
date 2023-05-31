module Chapter3.Leftist where

data Heap a = EHeap | THeap Int a (Heap a) (Heap a)
  deriving (Show, Eq, Ord)

empty :: Heap a
empty = EHeap

isEmpty :: Heap a -> Bool
isEmpty EHeap = True
isEmpty _ = False

rank :: Heap a -> Int
rank EHeap = 0
rank (THeap n _ _ _) = n

makeT :: a -> Heap a -> Heap a -> Heap a
makeT x a b
  | rank a >= rank b = THeap (rank b + 1) x a b
  | otherwise        = THeap (rank a + 1) x b a

merge :: Ord a => Heap a -> Heap a -> Heap a
merge EHeap h = h
merge h EHeap = h
merge h1@(THeap _ x a1 b1) h2@(THeap _ y a2 b2) =
  if x <= y then makeT x a1 (merge b1 h2)
  else makeT y a2 (merge h1 b2)

insertHeap :: Ord a => a -> Heap a -> Heap a
insertHeap x EHeap = THeap 1 x EHeap EHeap
insertHeap x t'@(THeap _ y l r) =
  if x <= y
  then makeT x EHeap t'
  else makeT y l (insertHeap x r)

insertHeap' :: Ord a => a -> Heap a -> Heap a
insertHeap' x h = merge (makeT x EHeap EHeap) h

findMin :: Heap a -> Maybe a
findMin EHeap = Nothing
findMin (THeap _ x _ _) = Just x

deleteMin :: Ord a => Heap a -> Maybe (Heap a)
deleteMin EHeap = Nothing
deleteMin (THeap _ _ l r) = Just (merge l r)

fromList :: Ord a => [a] -> Heap a
fromList xs =
    foldr merge empty 
      (map (\x -> THeap 1 x EHeap EHeap) xs)