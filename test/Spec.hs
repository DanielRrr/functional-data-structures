import Test.QuickCheck
import Chapter2.BinarySearch
import Chapter3.Leftist

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    v <- choose (0 :: Int, 1)
    if v == 0
      then pure E
      else T <$> arbitrary <*> arbitrary <*> arbitrary
  shrink E = []
  shrink (T l x r) =
    [ T l' x' r' | (l', x', r') <- shrink (l, x, r) ]

instance Arbitrary a => Arbitrary (Heap a) where
  arbitrary = do
    v <- choose (0 :: Int, 1)
    if v == 0
      then pure EHeap
      else THeap <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink EHeap = []
  shrink (THeap n x l r) =
    [ THeap n' x' l' r' | (n', x', l', r') <- shrink (n, x, l, r) ]

propMember :: Int -> Tree Int -> Bool
propMember x t = member x t == member' x t

propInsert :: Int -> Tree Int -> Bool
propInsert x t = insert x t == insert' x t

propInsertLeft :: Int -> Heap Int -> Bool
propInsertLeft t h = insertHeap t h == insertHeap' t h

main :: IO ()
main = do
  quickCheck propMember
  quickCheck propInsert
  quickCheck propInsertLeft
