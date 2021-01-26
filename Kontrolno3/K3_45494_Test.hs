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

module K3_45494_Test where

import Test.HUnit
import K3_45494

testTree = TestCase $ do
  assertEqual "empty tree"                False (treeContains EmptyTree "a")
  assertEqual "empty tree and string" True (treeContains EmptyTree "")
  assertEqual "empty string"             True (treeContains (Node 'a' EmptyTree EmptyTree) "")
  assertEqual "simple tree 1"             True (treeContains (Node 'a' (Node 'b' EmptyTree EmptyTree) EmptyTree) "a")
  assertEqual "simple tree 2"             True (treeContains (Node 'a' (Node 'b' EmptyTree EmptyTree) EmptyTree) "ab")
  assertEqual "simple tree 3"             True (treeContains (Node 'a' EmptyTree (Node 'b' EmptyTree EmptyTree)) "a")
  assertEqual "simple tree 4"             True (treeContains (Node 'a' EmptyTree (Node 'b' EmptyTree EmptyTree)) "ab")

  assertEqual "fail tree 1"                  False (treeContains (Node 'a' EmptyTree (Node 'b' EmptyTree EmptyTree)) "abc")
  assertEqual "fail tree 2"                  False (treeContains (Node 'a' EmptyTree (Node 'b' EmptyTree EmptyTree)) "zx")
  assertEqual "fail tree 3"                  False (treeContains (Node 'a' (Node 'b' EmptyTree EmptyTree) EmptyTree) "abc")
  assertEqual "fail tree 4"                  False (treeContains (Node 'a' (Node 'b' EmptyTree EmptyTree) EmptyTree) "zx")

  assertEqual "complex tree 1"           True (treeContains
    (Node 'a'
      (Node 'b'
        (Node 'd' EmptyTree
                    (Node 'g' EmptyTree EmptyTree))
        (Node 'e' EmptyTree EmptyTree))
      (Node 'c' EmptyTree(Node 'f' EmptyTree EmptyTree))) "abdg")

  assertEqual "complex tree 2"          False (treeContains
    (Node 'a'
      (Node 'b'
        (Node 'd' EmptyTree
                    (Node 'g' EmptyTree EmptyTree))
        (Node 'e' EmptyTree EmptyTree))
      (Node 'c' EmptyTree(Node 'f' EmptyTree EmptyTree))) "gdba")

  assertEqual "complex tree 3"         False (treeContains
    (Node 'a'
      (Node 'b'
        (Node 'd' EmptyTree
                    (Node 'g' EmptyTree EmptyTree))
        (Node 'e' EmptyTree EmptyTree))
      (Node 'c' EmptyTree(Node 'f' EmptyTree EmptyTree))) "bcf")


testInjective = TestCase $ do
  assertEqual "single element interval"    True (isInjective (+1) 1 1)
  assertEqual "two element interval true" True (isInjective (+1) 1 2)
  assertEqual "two element interval false" False (isInjective (\x -> 0) 1 2)

  assertEqual "example true"                   True (isInjective (\x -> x - 1) 0 100)
  assertEqual "example false"                  False (isInjective (\x -> 100) 0 100)
  
  assertEqual "power example true"          True (isInjective (\x -> x^1) 0 100)
  assertEqual "power  example false"        False (isInjective (\x -> x^0) 0 100)

  assertEqual "modulo example true"        True (isInjective (\x -> x `mod` 101) 0 100)
  assertEqual "modulo example false"       False (isInjective (\x -> x `mod` 2) 0 100)
  assertEqual "modulo example false 2"    False (isInjective (\x -> x `mod` 1) 0 100)
  assertEqual "modulo example false 3"    False (isInjective (\x -> x `mod` 100) 0 100)


tl = TestList [testTree, testInjective]

main = do
  runTestTT tl
