{--
  СУ "Св. Климент Охридски"
  Факултет по математика и информатика
  Курс Функционално програмиране 2020/21
  Контролно 3
  2021-01-16

  Име: Людмил Данаилов
  ФН: 45494
  Специалност: Информатика
  Курс: 3
  Административна група: 3
  Начален час на контролното: 9:30
--}

module K3_45494 where

import Data.List (nub, (\\))

data Tree a = EmptyTree | Node {
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a} deriving (Show, Read)


treeContains :: Tree Char -> String -> Bool
treeContains _ [] = True
treeContains EmptyTree _ = False
treeContains (Node a ltree rtree) str
  | a == head str = treeContains ltree (tail str) || treeContains rtree (tail str)
  | otherwise = treeContains ltree str || treeContains rtree str


isInjective :: Integral t => (t -> t) -> t -> t -> Bool
isInjective f a b = appliedList \\ nubbedList == []
  where appliedList = [ (f x) | x <- [a..b] ]
        nubbedList = nub appliedList
