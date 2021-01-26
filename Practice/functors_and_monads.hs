data Tree a =
    Node a (Tree a) (Tree a)
    | EmptyTree
    deriving (Show)

instance Functor Tree where
    fmap _ EmptyTree = EmptyTree
    fmap f (Node x ltree rtree) = Node (f x) (fmap f ltree) (fmap f rtree)

lookup4 :: String -> [(String, String)] -> Maybe String
lookup4 key lst = do
    v1 <- lookup key lst
    v2 <- lookup v1 lst
    v3 <- lookup v2 lst
    v4 <- lookup v3 lst
    return v4

ifJust :: Maybe String -> (String -> Maybe String) -> Maybe String
ifJust Nothing _ = Nothing
ifJust (Just k) f = f k

main'' = do
    five <- Just 5
    Just $ 4 + five
