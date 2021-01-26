import Task1
import Test.HUnit

testEmpty :: Test
testEmpty = TestCase $ do
    assertBool "Empty tree 1" (null (values Inorder EmptyTree))
    assertBool "Empty tree 2" (null (values Postorder EmptyTree))
    assertBool "Empty tree 3" (null (values Preorder EmptyTree))

testInorder :: Test
testInorder = TestCase $ do
    assertEqual "Simple Inorder Tree 1" [5] (values Inorder (Node 5 EmptyTree EmptyTree))
    assertEqual "Complex Inorder tree 2" [2, 22, 6, 5, 1, 111, 3] (
        values Inorder (
            Node 5
                (Node 22 (Node 2 EmptyTree EmptyTree)(Node 6 EmptyTree EmptyTree))
                (Node 1 EmptyTree (Node 3 (Node 111 EmptyTree EmptyTree) EmptyTree))))
    assertEqual "Complex Inorder tree 3" [4, 2, 5, 1, 3] (
        values Inorder (
            Node 1
                (Node 2 (Node 4 EmptyTree EmptyTree)(Node 5 EmptyTree EmptyTree))
                (Node 3 EmptyTree EmptyTree)))

testPostOrder :: Test
testPostOrder = TestCase $ do
    assertEqual "Simple Postorder Tree 1" [5] (values Postorder (Node 5 EmptyTree EmptyTree))
    assertEqual "Complex Postorder tree 2" [2, 6, 22, 111, 3, 1, 5] (
        values Postorder (
            Node 5
                (Node 22 (Node 2 EmptyTree EmptyTree)(Node 6 EmptyTree EmptyTree))
                (Node 1 EmptyTree (Node 3 (Node 111 EmptyTree EmptyTree) EmptyTree))))
    assertEqual "Complex Postorder tree 3" [4, 5, 2, 3, 1] (
        values Postorder (
            Node 1
                (Node 2 (Node 4 EmptyTree EmptyTree)(Node 5 EmptyTree EmptyTree))
                (Node 3 EmptyTree EmptyTree)))

testPreOrder :: Test
testPreOrder = TestCase $ do
    assertEqual "Simple Preorder Tree 1" [5] (values Preorder (Node 5 EmptyTree EmptyTree))
    assertEqual "Complex Preorder tree 2" [5, 22, 2, 6, 1, 3, 111] (
        values Preorder (
            Node 5
                (Node 22 (Node 2 EmptyTree EmptyTree)(Node 6 EmptyTree EmptyTree))
                (Node 1 EmptyTree (Node 3 (Node 111 EmptyTree EmptyTree) EmptyTree))))
    assertEqual "Complex Preorder tree 3" [1, 2, 4, 5, 3] (
        values Preorder (
            Node 1
                (Node 2 (Node 4 EmptyTree EmptyTree)(Node 5 EmptyTree EmptyTree))
                (Node 3 EmptyTree EmptyTree)))



tl = TestList [testEmpty, testInorder, testPostOrder, testPreOrder]
main = runTestTT tl
