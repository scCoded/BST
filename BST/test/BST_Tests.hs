import Test.HUnit
import Test.QuickCheck
import BST

main :: IO ()
main = do
    results <- runTestTT tests
    print results

-- Create Empty Tree
-- createEmptyTreeTest :: Test
-- createEmptyTreeTest = TestCase (assertEqual "createEmptyTree" Leaf createEmptyTree)

-- insertNode() unit tests
insertNodeInEmptyTreeTest :: Test
insertNodeInEmptyTreeTest = TestCase (assertEqual "insert 1st node into new, empty tree"
    (Node 1 "value" Leaf Leaf)
    (insertNode 1 "value" createEmptyTree)
    )

insertStringKeyAndValueTest :: Test
insertStringKeyAndValueTest = TestCase (assertEqual "insert string key/value type"
    (Node "key" "value" Leaf Leaf)
    (insertNode "key" "value" createEmptyTree)
    )

insertIntKeyAndValueTest :: Test
insertIntKeyAndValueTest = TestCase (assertEqual "insert int key/value type"
    (Node 1 "value" Leaf Leaf)
    (insertNode 1 "value" createEmptyTree)
    )

insertDifferentKeyAndValueTest :: Test
insertDifferentKeyAndValueTest = TestCase (assertEqual "insert different key/value type"
    (Node 1 "value" Leaf Leaf)
    (insertNode 1 "value" createEmptyTree)
    )

insertNodeWithSameKeyTest :: Test
tree = createTreeFromList [(7,"7"), (8,"8"), (9,"9"), (10,"10")]
insertNodeWithSameKeyTest = TestCase (assertEqual "insert node with same key to update it's value"
    (Node 7 "updated" Leaf (Node 8 "8" Leaf (Node 9 "9" Leaf (Node 10 "10" Leaf Leaf))))
    (insertNode 7 "updated" tree)
    )

insertNewNode :: Test
insertNewNode = TestCase (assertEqual "insert new node key"
    (Node 7 "7" (Node 2 "newValue" Leaf Leaf) (Node 8 "8" Leaf (Node 9 "9" Leaf (Node 10 "10" Leaf Leaf))))
    (insertNode 2 "newValue" tree)
    )


createLeftSkewedTreeTest :: Test
leftSkewedTree = createTreeFromList [(10, "10"), (8, "8"), (4, "4"), (2, "2"), (1, "1")]
createLeftSkewedTreeTest = TestCase (assertEqual "create left skewed tree, by inserting increasingly smaller keys"
    (Node 10 "10" (Node 8 "8" (Node 4 "4" (Node 2 "2" (Node 1 "1" Leaf Leaf) Leaf) Leaf) Leaf) Leaf)
    leftSkewedTree
    )

createRightSkewedTreeTest :: Test
rightSkewedTree = createTreeFromList [(10, "10"), (12, "12"), (14, "14"), (16, "16"), (18, "18"), (20, "20")]
createRightSkewedTreeTest = TestCase (assertEqual "create right skewed tree, by inserting increasingly larger keys"
    (Node 10 "10" Leaf (Node 12 "12" Leaf (Node 14 "14" Leaf (Node 16 "16" Leaf (Node 18 "18" Leaf (Node 20 "20" Leaf Leaf))))))
    rightSkewedTree
    )

createFullTree :: Test
fullTree = createTreeFromList [(10, "10"), (12, "12"), (8, "8"), (7, "7"), (14, "14"), (9, "9"), (20, "20"), (1, "1")]
createFullTree = TestCase (assertEqual "create full tree"
    (Node 10 "10" (Node 8 "8" (Node 7 "7" (Node 1 "1" Leaf Leaf) Leaf) (Node 9 "9" Leaf Leaf)) (Node 12 "12" Leaf (Node 14 "14" Leaf (Node 20 "20" Leaf Leaf))))
    fullTree
    )

-- insertNodesFromList unit tests
insertNodesFromListTest :: Test
insertNodesFromListTest = TestCase (assertEqual "insert nodes from list"
    (Node 7 "7" Leaf (Node 8 "8" Leaf (Node 9 "9" Leaf (Node 10 "10" Leaf (Node 11 "11" Leaf (Node 12 "12" Leaf (Node 13 "13" Leaf Leaf)))))))
    (insertNodesFromList [(7,"7"), (8,"8"), (9,"9"), (10,"10"), (11,"11"), (12,"12"), (13,"13")] createEmptyTree)
    )

-- createTreeFromList 
createTreeFromListTest :: Test
createTreeFromListTest = TestCase (assertEqual "create tree from list"
    (Node 7 "7" Leaf (Node 8 "8" Leaf (Node 9 "9" Leaf (Node 10 "10" Leaf (Node 11 "11" Leaf (Node 12 "12" Leaf (Node 13 "13" Leaf Leaf)))))))
    (createTreeFromList [(7,"7"), (8,"8"), (9,"9"), (10,"10"), (11,"11"), (12,"12"), (13,"13")])
    )

-- getValue tests
getRootValue :: Test
getRootValue = TestCase (assertEqual "get root value"
    (Just "7")
    (getValue 7 tree)
    )

getLeftMostValue :: Test
getLeftMostValue = TestCase (assertEqual "get left most value"
    (Just "1")
    (getValue 1 fullTree)
    )

getRightMostValue :: Test
getRightMostValue = TestCase (assertEqual "get right most value"
    (Just "20")
    (getValue 20 fullTree)
    )

getValueThatDoesntExist :: Test
getValueThatDoesntExist = TestCase (assertEqual "get value that doesn't exist"
    (Nothing)
    (getValue 100 fullTree)
    )

-- removeNode unit tests
removeNodeWithLeftChildOnly :: Test
removeNodeWithLeftChildOnly = TestCase (assertEqual "remove node with left child only"
    (Node 10 "10" (Node 8 "8" (Node 1 "1" Leaf Leaf) (Node 9 "9" Leaf Leaf)) (Node 12 "12" Leaf (Node 14 "14" Leaf (Node 20 "20" Leaf Leaf))))
    (removeNode 7 fullTree)
    )

removeNodeWithRightChildOnly :: Test
removeNodeWithRightChildOnly = TestCase (assertEqual "remove node with right child only"
    (Node 10 "10" (Node 8 "8" (Node 7 "7" (Node 1 "1" Leaf Leaf) Leaf) (Node 9 "9" Leaf Leaf)) (Node 12 "12" Leaf (Node 20 "20" Leaf Leaf)))
    (removeNode 14 fullTree)
    )

removeNodeWithTwoChildren :: Test
removeNodeWithTwoChildren = TestCase (assertEqual "remove node with two children"
    (Node 10 "10" (Node 9 "9" (Node 7 "7" (Node 1 "1" Leaf Leaf) Leaf) Leaf) (Node 12 "12" Leaf (Node 14 "14" Leaf (Node 20 "20" Leaf Leaf))))
    (removeNode 8 fullTree)
    )

removeNodeWithNoChildren :: Test
removeNodeWithNoChildren = TestCase (assertEqual "remove node with no children"
    (Node 10 "10" (Node 8 "8" (Node 7 "7" (Node 1 "1" Leaf Leaf) Leaf) Leaf) (Node 12 "12" Leaf (Node 14 "14" Leaf (Node 20 "20" Leaf Leaf))))
    (removeNode 9 fullTree)
    )

removeNodeThatDoesntExist :: Test
removeNodeThatDoesntExist = TestCase (assertEqual "remove node that doesn't exist"
    (fullTree)
    (removeNode 100 fullTree)
    )

-- removeIf unit tests
removeIfKeyGreaterThanTen :: Test
removeIfKeyGreaterThanTen = TestCase (assertEqual "remove if key greater than ten"
    (Node 10 "10" (Node 8 "8" (Node 7 "7" (Node 1 "1" Leaf Leaf) Leaf) (Node 9 "9" Leaf Leaf)) Leaf)
    (removeIf (>10) fullTree)
    )

removeIfKeyLessThanTwenty :: Test
removeIfKeyLessThanTwenty = TestCase (assertEqual "remove if key less than twenty"
    (Node 20 "20" Leaf Leaf)
    (removeIf (<20) fullTree)
    )
    
-- getListOfEntries unit tests
getAllEntriesTest :: Test
getAllEntriesTest = TestCase (assertEqual "get all entries"
    [(1,"1"),(7,"7"),(8,"8"),(9,"9"),(10,"10"),(12,"12"),(14,"14"),(20,"20")]
    (getListOfEntries fullTree)
    )

getAllEntriesOnEmptyTree :: Test
getAllEntriesOnEmptyTree = TestCase (assertEqual "get all entries on empty tree"
    ([] :: [(Int, String)])
    (getListOfEntries Leaf)
    )


tests :: Test
tests = TestList [
        insertNodeInEmptyTreeTest,
        insertStringKeyAndValueTest,
        insertIntKeyAndValueTest,
        insertDifferentKeyAndValueTest,
        insertNodeWithSameKeyTest,
        insertNewNode,
        createLeftSkewedTreeTest,
        createRightSkewedTreeTest,
        createFullTree,
        insertNodesFromListTest,
        createTreeFromListTest,
        getRootValue,
        getLeftMostValue,
        getRightMostValue,
        getValueThatDoesntExist,
        removeNodeWithLeftChildOnly,
        removeNodeWithRightChildOnly,
        removeNodeWithTwoChildren,
        removeNodeWithNoChildren,
        removeNodeThatDoesntExist,
        removeIfKeyGreaterThanTen,
        removeIfKeyLessThanTwenty,
        getAllEntriesTest,
        getAllEntriesOnEmptyTree
    ]