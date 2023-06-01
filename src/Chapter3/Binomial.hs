module Chapter3.Binomial where

data BinomTree a = Node Int a ([BinomTree a])
  deriving Show

type Heap a = [BinomTree a]

singleton :: a -> BinomTree a
singleton x = Node 0 x []

-- | Each list of children is maintained in decreasing 
-- order of rank, and elements are stored in heap order. 
-- We maintain heap order by always linking trees with larger roots under trees with smaller roots.
link :: Ord a => BinomTree a -> BinomTree a -> BinomTree a
link t1@(Node r x xs) t2@(Node _ y ys) =
  if x <= y then Node (r + 1) x (t2:xs)
  else Node (r + 1) y (t1:ys)

-- | BinomTree rank
rank :: BinomTree a -> Int
rank (Node r _ _) = r

-- | Tree root
root :: BinomTree a -> a
root (Node _ x _) = x

-- | inserts a tree via rank checking
insTree :: Ord a => BinomTree a -> Heap a -> Heap a
insTree t [] = [t]
insTree t1 ts@(t2:ts') =
  if rank t1 < rank t2 then t1:ts else
  insTree (link t1 t2) ts'

insertNode :: Ord a => a -> Heap a -> Heap a
insertNode x h = insTree (singleton x) h

-- | merges two heaps recursively by comparing ranks
merge :: Ord a => Heap a -> Heap a -> Heap a
merge [] h2 = h2
merge h1 [] = h1
merge h1@(t1:ts1) h2@(t2:ts2) =
  if rank t1 < rank t2 then merge ts1 h2
  else if rank t2 < rank t2 then t2 : merge h1 ts2
  else insTree (link t1 t2) (merge ts1 ts2)

-- | removes a minimal tree, that is, 
-- a tree with a minimal root amongst elements of a given heap
removeMinTree :: Ord a => Heap a -> Maybe (BinomTree a, Heap a)
removeMinTree [] = Nothing
removeMinTree [t] = Just (t, [])
removeMinTree (t:ts) = do
  (t', ts') <- removeMinTree ts
  if root t <= root t' then
    return (t,ts)
  else return (t', t:ts')

-- | Find mip
findMin :: Ord a => Heap a -> a
findMin xs = minimum (map root xs)