import Test.HUnit
import Test.QuickCheck
import BST
    ( createEmptyTree,
      getListOfEntries,
      getValue,
      insertNode,
      removeIf,
      removeNode,
      BST(Leaf, Node) )

main :: IO ()
main = do
    results <- runTestTT tests
    print results

-- insertNode() unit tests
insertNodeInEmptyTreeTest :: Test
tree = createEmptyTree
insertNodeInEmptyTreeTest = TestCase (assertEqual "insert 1st node into new, empty tree"
    (Node 1 "value" Leaf Leaf)
    (insertNode 1 "value" tree)
    )

insertNodeWithSameKeyTest :: Test
tree2 = createEmptyTree
tree3 = insertNode 1 "value" tree2
insertNodeWithSameKeyTest = TestCase (assertEqual "insert node with same key"
    (Node 1 "updated" Leaf Leaf)
    (insertNode 1 "updated" tree3)
    )

insertNodeWithBiggerKeyTest :: Test
tree4 = createEmptyTree
tree5 = insertNode 1 "value" tree4
insertNodeWithBiggerKeyTest = TestCase (assertEqual "insert node with bigger key"
    (Node 1 "value" Leaf (Node 2 "bigger" Leaf Leaf))
    (insertNode 2 "bigger" tree5)
    )

insertNodeWithSmallerKeyTest :: Test
tree6 = createEmptyTree
tree7 = insertNode 5 "value" tree6
insertNodeWithSmallerKeyTest = TestCase (assertEqual "insert node with smaller key"
    (Node 5 "value" (Node 2 "smaller" Leaf Leaf) Leaf)
    (insertNode 2 "smaller" tree7)
    )

createLeftSkewedTreeTest :: Test
tree8 = createEmptyTree
tree9 = insertNode 10 "10" tree8
tree10 = insertNode 8 "8" tree9
tree11 = insertNode 4 "4" tree10
tree12 = insertNode 2 "2" tree11
tree13 = insertNode 1 "1" tree12
createLeftSkewedTreeTest = TestCase (assertEqual "create left skewed tree, by inserting increasingly smaller keys"
    (Node 10 "10" (Node 8 "8" (Node 4 "4" (Node 2 "2" (Node 1 "1" Leaf Leaf) Leaf) Leaf) Leaf) Leaf)
    tree13
    )

createRightSkewedTreeTest :: Test
tree14 = createEmptyTree
tree15 = insertNode 10 "10" tree14
tree16 = insertNode 12 "12" tree15
tree17 = insertNode 14 "14" tree16
tree18 = insertNode 16 "16" tree17
tree19 = insertNode 18 "18" tree18
tree20 = insertNode 20 "20" tree19
createRightSkewedTreeTest = TestCase (assertEqual "create right skewed tree, by inserting increasingly larger keys"
    (Node 10 "10" Leaf (Node 12 "12" Leaf (Node 14 "14" Leaf (Node 16 "16" Leaf (Node 18 "18" Leaf (Node 20 "20" Leaf Leaf))))))
    tree20
    )

createBalancedTreeTest :: Test
tree21 = createEmptyTree
tree22 = insertNode 10 "10" tree21
tree23 = insertNode 12 "12" tree22
tree24 = insertNode 8 "8" tree23
tree25 = insertNode 7 "7" tree24
tree26 = insertNode 9 "9" tree25
tree27 = insertNode 11 "11" tree26  
tree28 = insertNode 13 "13" tree27
createBalancedTreeTest = TestCase (assertEqual "create balanced tree"
    (Node 10 "10" (Node 8 "8" (Node 7 "7" Leaf Leaf) (Node 9 "9" Leaf Leaf)) (Node 12 "12" (Node 11 "11" Leaf Leaf) (Node 13 "13" Leaf Leaf)))
    tree28
    )



-- getValue() unit tests
getValidValueTest :: Test
getValidValueTest = TestCase (assertEqual "get valid value"
    "value"
    (getValue 1 tree5)
    )

getReplacedValueTest :: Test 
updatedTree3 = insertNode 1 "updated" tree3
getReplacedValueTest = TestCase (assertEqual "get replaced value"
    "updated"
    (getValue 1 updatedTree3)
    )

getRootValueTest :: Test
getRootValueTest = TestCase (assertEqual "get root value"
    "10"
    (getValue 10 tree20)
    )

getChildValueTest :: Test
getChildValueTest = TestCase (assertEqual "get child value"
    "13"
    (getValue 13 tree28)
    )

--getInvalidValueTest :: Test
--getInvalidValueTest = TestCase (assertEqual "get invalid value from populated tree"
--    ("Key not found")
--   (evaluate(getValue 100 tree5))
--   )

 --getInvalidValueTest2:: Test
--getInvalidValueTest2 = TestCase (assertException "get invalid value from empty tree"
   -- ("Key not found")
 --   (getValue 100 tree)
 --   )

-- removeNode() unit tests
removeNodeRootTest :: Test
removeNodeRootTest = TestCase (assertEqual "remove root node"
    (Leaf)
    (removeNode 1 tree3)
    ) 

removeNodeRootWithChildTest :: Test
tree29 = createEmptyTree
tree30 = insertNode 1 "root" tree29
tree31 = insertNode 2 "child" tree30
removeNodeRootWithChildTest = TestCase (assertEqual "remove root node, and set child node to root"
    (Node 2 "child" Leaf Leaf)
    (removeNode 1 tree31)
    )

removeNodeChildTest :: Test
removeNodeChildTest = TestCase (assertEqual "remove child node"
    (Node 1 "root" Leaf Leaf)
    (removeNode 2 tree31)
    )

removeNodeInnerParentTest :: Test
tree32 = createEmptyTree
tree33 = insertNode 1 "root" tree32
tree34 = insertNode 3 "parent" tree33
tree35 = insertNode 2 "child" tree34
tree36 = insertNode 4 "child" tree35
removeNodeInnerParentTest = TestCase (assertEqual "remove inner parent node, minimum key from right subtree becomes new parent"
    (Node 1 "root" Leaf (Node 4 "child" (Node 2 "child" Leaf Leaf) Leaf))
    (removeNode 3 tree36)
    )

-- removeNodeFromEmptyTreeTest :: Test
-- emptyTree = createEmptyTree
-- removeNodeFromEmptyTreeTest = TestCase (assertEqual "remove node from empty tree"
--     (Leaf)
--     (removeNode 1 emptyTree)
--     )

-- removeNodeInvalidTest :: Test

-- removeIf() unit tests
removeIfEvenTest :: Test
removeIfEvenTest = TestCase (assertEqual "remove even nodes, tree used comprises entirely of even nodes, so should become empty"
    (Leaf)
    (removeIf even tree20)
    )

removeIfOddTest :: Test
removeIfOddTest = TestCase (assertEqual "remove odd nodes, should leave only even nodes"
    (Node 10 "10" (Node 8 "8" Leaf Leaf) (Node 12 "12" Leaf Leaf))
    (removeIf odd tree28)
    )

removeIfMultipleOf3Test :: Test
removeIfMultipleOf3Test = TestCase (assertEqual "remove nodes that are multiples of 3"
    (Node 10 "10" (Node 8 "8" (Node 7 "7" Leaf Leaf) Leaf) (Node 13 "13" (Node 11 "11" Leaf Leaf) Leaf))
    (removeIf (\x -> x `mod` 3 == 0) tree28)
    )

 -- getListOfEntriesTest() unit tests
showEmptyTreeTest :: Test
showEmptyTreeTest = TestCase (assertEqual "check empty tree has no entries"
    0
    (length (getListOfEntries tree))
    )

showEmptyTreeTest2 :: Test
showEmptyTreeTest2 = TestCase (assertEqual "check empty tree shows has no entries"
    ([] :: [(Int, String)])
    (getListOfEntries createEmptyTree)
    )

showPopulatedTreeTest :: Test
showPopulatedTreeTest = TestCase (assertEqual "check populated tree has correct number of entries"
    7
    (length (getListOfEntries tree28))
    )

showPopulatedTreeTest2 :: Test
showPopulatedTreeTest2 = TestCase (assertEqual "check populate tree shows correct entries"
    [(7,"7"), (8,"8"), (9,"9"), (10,"10"), (11,"11"), (12,"12"), (13,"13")]
    (getListOfEntries tree28)
    )

tests :: Test
tests = TestList [
    insertNodeInEmptyTreeTest,
    insertNodeWithSameKeyTest,
    insertNodeWithBiggerKeyTest,
    insertNodeWithSmallerKeyTest,
    createLeftSkewedTreeTest,
    createRightSkewedTreeTest,
    createBalancedTreeTest,
    getValidValueTest,
    getReplacedValueTest,
    getRootValueTest,
    getChildValueTest,
    removeNodeRootTest,
    removeNodeRootWithChildTest,
    removeNodeChildTest,
    removeNodeInnerParentTest,
    removeIfEvenTest,
    removeIfOddTest,
    removeIfMultipleOf3Test,
    showEmptyTreeTest,
    showEmptyTreeTest2,
    showPopulatedTreeTest,
    showPopulatedTreeTest2
    ]