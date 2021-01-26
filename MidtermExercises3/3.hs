import Data.List (isPrefixOf)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (h:_) = Just h

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:t) = Just t

safeUncons :: [a] -> Maybe (a, [a])
safeUncons [] = Nothing
safeUncons (h:t) = Just (h, t)

findIndex :: (Eq a) => a  -> [a] -> Maybe Int
findIndex x lst
  | null lst = Nothing
  | x == (head lst) = Just 0
  | otherwise = (+1) <$> (findIndex x (tail lst))

stripPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripPrefix lst1 lst2
  | isPrefixOf lst1 lst2 = Just (drop (length lst1) lst2)
  | otherwise = Nothing

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

mapMaybe :: (a -> a) -> Maybe a -> Maybe a
mapMaybe f (Just x) = Just (f x)
mapMaybe _ Nothing = Nothing

data Tree a = EmptyTree
  | Node {
    value :: a,
    leftTree :: Tree a,
    rightTree :: Tree a
  } deriving (Show, Read)

maxSumPath :: (Ord a, Floating a) => Tree a -> a
maxSumPath EmptyTree = 0
maxSumPath (Node a left right) = (max (a + maxSumPath left) (a + maxSumPath right))

hasEmptyLeaves :: Tree a -> Bool
hasEmptyLeaves (Node _ EmptyTree EmptyTree) = True
hasEmptyLeaves _ = False

prune :: Tree a -> Tree a
prune EmptyTree = EmptyTree
prune tree
  | hasEmptyLeaves tree = EmptyTree
  | otherwise = (Node (value tree) (prune (leftTree tree)) (prune (rightTree tree)))

bloom :: Tree a -> Tree a
bloom EmptyTree = EmptyTree
bloom tree
  | hasEmptyLeaves tree = (Node (value tree) tree tree)
  | otherwise = (Node (value tree) (bloom (leftTree tree)) (bloom(rightTree tree)))

rotateLeft, rotateRight :: Tree a -> Tree a
rotateLeft (Node p a (Node q b c)) = Node q (Node p a b) c
rotateLeft t = t
rotateRight (Node q (Node p a b) c) = Node p a (Node q b c)
rotateRight t = t

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ EmptyTree = EmptyTree
treeMap f (Node a left right) =
  (Node (f a) (treeMap f left) (treeMap f right))

instance Functor Tree where
  fmap = treeMap

data BST a = BSTEmpty
  | BSTNode a (BST a) (BST a) deriving (Show, Read)

bstValues :: BST a -> [a]
bstValues BSTEmpty = []
bstValues (BSTNode a left right) = bstValues left ++ [a] ++ bstValues right

bstInsert :: (Eq a, Ord a) => a -> BST a -> BST a
bstInsert x BSTEmpty = (BSTNode x BSTEmpty BSTEmpty)
bstInsert x (BSTNode a left right)
  | x < a = (BSTNode a (bstInsert x left) right)
  | x > a = (BSTNode a left (bstInsert x right))
  | otherwise = (BSTNode a left right)

--bstSort :: (Ord a) => [a] -> [a]
bstSort lst =
  let bst = foldr bstInsert BSTEmpty lst
  in bstValues bst

main'' =
  let a =  (Node 5
                (Node 22 (Node 2 EmptyTree EmptyTree)(Node 6 EmptyTree EmptyTree))
                (Node 1 EmptyTree (Node 3 (Node 111 EmptyTree EmptyTree) EmptyTree)))
  in bloom a
