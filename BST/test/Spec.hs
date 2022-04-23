import Test.HUnit
import Test.HUnit.Tools
import Test.QuickCheck
import Lib

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
    showEmptyTreeTest,
    showEmptyTreeTest2,
    showPopulatedTreeTest,
    showPopulatedTreeTest2
    ]