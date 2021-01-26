data BinTree a
    = Empty
    | Node a (BinTree a) (BinTree a)
    deriving (Show)

pushBST :: Ord a => a -> BinTree a -> BinTree a
pushBST el Empty = Node el Empty Empty
pushBST el t@(Node root left right)
  | el == root = t
  | el > root = Node root left (pushBST el right)
  | otherwise = Node root (pushBST el left) right

elemBST :: Ord a => a -> BinTree a -> Bool
elemBST _ Empty = False
elemBST a (Node root left right)
    | a == root = True
    | a > root = elemBST a right
    | otherwise = elemBST a left
