module Task1 where

data Tree a =
  EmptyTree
  | Node {
    value :: a,
    left :: Tree a,
    right :: Tree a
  } deriving (Show, Read)

data Strategy = Inorder | Postorder | Preorder deriving (Show, Read)

values :: Strategy -> (Tree a) -> [a]
values _ EmptyTree = []
values Inorder tree = values Inorder (left tree) ++ [value tree] ++ values Inorder (right tree)
values Preorder tree = [value tree] ++ values Preorder (left tree) ++ values Preorder (right tree)
values Postorder tree = values Postorder (left tree) ++ values Postorder (right tree) ++ [value tree]
