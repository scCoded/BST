module BST_Tests where

import Test.HUnit as HUnit
import BST
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.Maybe (isJust, isNothing)

-- Create Empty Tree
createEmptyTreeTest ::  HUnit.Assertion
createEmptyTreeTest = (HUnit.assertEqual "createEmptyTree" 
    (Leaf :: BST Int Int)
    createEmptyTree
    )

-- Create Node
createNodeTest :: HUnit.Assertion
createNodeTest = HUnit.assertEqual "create node" 
    (Node 1 1 Leaf Leaf)
    (createNode 1 1)

-- insertNode tests
insertNodeInEmptyTreeTest :: HUnit.Assertion
insertNodeInEmptyTreeTest = HUnit.assertEqual "insert 1st node into new, empty tree"
    (Node 1 "value" Leaf Leaf)
    (insertNode 1 "value" createEmptyTree)

insertNewNode :: HUnit.Assertion
tree = createTreeFromList [(7,"7"), (8,"8"), (9,"9"), (10,"10")]
insertNewNode = HUnit.assertEqual "insert new node key"
    (Node 7 "7" (Node 2 "newValue" Leaf Leaf) (Node 8 "8" Leaf (Node 9 "9" Leaf (Node 10 "10" Leaf Leaf))))
    (insertNode 2 "newValue" tree)

insertStringKeyAndValueTest :: HUnit.Assertion
insertStringKeyAndValueTest = HUnit.assertEqual "insert string key/value type"
    (Node "key" "value" Leaf Leaf)
    (insertNode "key" "value" createEmptyTree)

insertIntKeyAndValueTest :: HUnit.Assertion
insertIntKeyAndValueTest = HUnit.assertEqual "insert int key/value type"
    (Node 1 "value" Leaf Leaf)
    (insertNode 1 "value" createEmptyTree)

insertDifferentKeyAndValueTest :: HUnit.Assertion
insertDifferentKeyAndValueTest = HUnit.assertEqual "insert different key/value type"
    (Node 1 "value" Leaf Leaf)
    (insertNode 1 "value" createEmptyTree)

insertNodeWithSameKeyTest :: HUnit.Assertion
insertNodeWithSameKeyTest = HUnit.assertEqual "insert node with same key to update it's value"
    (Node 7 "updated" Leaf (Node 8 "8" Leaf (Node 9 "9" Leaf (Node 10 "10" Leaf Leaf))))
    (insertNode 7 "updated" tree)

createLeftSkewedTreeTest :: HUnit.Assertion
leftSkewedTree = createTreeFromList [(10, "10"), (8, "8"), (4, "4"), (2, "2"), (1, "1")]
createLeftSkewedTreeTest = HUnit.assertEqual "create left skewed tree, by inserting increasingly smaller keys"
    (Node 10 "10" (Node 8 "8" (Node 4 "4" (Node 2 "2" (Node 1 "1" Leaf Leaf) Leaf) Leaf) Leaf) Leaf)
    leftSkewedTree

createRightSkewedTreeTest :: HUnit.Assertion
rightSkewedTree = createTreeFromList [(10, "10"), (12, "12"), (14, "14"), (16, "16"), (18, "18"), (20, "20")]
createRightSkewedTreeTest = HUnit.assertEqual "create right skewed tree, by inserting increasingly larger keys"
    (Node 10 "10" Leaf (Node 12 "12" Leaf (Node 14 "14" Leaf (Node 16 "16" Leaf (Node 18 "18" Leaf (Node 20 "20" Leaf Leaf))))))
    rightSkewedTree

createFullTree :: HUnit.Assertion
fullTree = createTreeFromList [(10, "10"), (12, "12"), (8, "8"), (7, "7"), (14, "14"), (9, "9"), (20, "20"), (1, "1")]
createFullTree = HUnit.assertEqual "create full tree"
    (Node 10 "10" (Node 8 "8" (Node 7 "7" (Node 1 "1" Leaf Leaf) Leaf) (Node 9 "9" Leaf Leaf)) (Node 12 "12" Leaf (Node 14 "14" Leaf (Node 20 "20" Leaf Leaf))))
    fullTree

-- insertNodesFromList tests
insertNodesFromListTest :: HUnit.Assertion
insertNodesFromListTest = HUnit.assertEqual "insert nodes from list"
    (Node 7 "7" Leaf (Node 8 "8" Leaf (Node 9 "9" Leaf (Node 10 "10" Leaf (Node 11 "11" Leaf (Node 12 "12" Leaf (Node 13 "13" Leaf Leaf)))))))
    (insertNodesFromList [(7,"7"), (8,"8"), (9,"9"), (10,"10"), (11,"11"), (12,"12"), (13,"13")] createEmptyTree)

-- createTreeFromList 
createTreeFromListTest :: HUnit.Assertion
createTreeFromListTest = HUnit.assertEqual "create tree from list"
    (Node 7 "7" Leaf (Node 8 "8" Leaf (Node 9 "9" Leaf (Node 10 "10" Leaf (Node 11 "11" Leaf (Node 12 "12" Leaf (Node 13 "13" Leaf Leaf)))))))
    (createTreeFromList [(7,"7"), (8,"8"), (9,"9"), (10,"10"), (11,"11"), (12,"12"), (13,"13")])

-- getValue tests
getRootValue :: HUnit.Assertion
getRootValue = HUnit.assertEqual "get root value"
    (Just "7")
    (getValue 7 tree)

getLeftMostValue :: HUnit.Assertion
getLeftMostValue = HUnit.assertEqual "get left most value"
    (Just "1")
    (getValue 1 fullTree)

getRightMostValue :: HUnit.Assertion
getRightMostValue = HUnit.assertEqual "get right most value"
    (Just "20")
    (getValue 20 fullTree)

getValueThatDoesntExist :: HUnit.Assertion
getValueThatDoesntExist = HUnit.assertEqual "get value that doesn't exist"
    (Nothing)
    (getValue 100 fullTree)

-- removeNode tests
removeNodeWithLeftChildOnly :: HUnit.Assertion
removeNodeWithLeftChildOnly = HUnit.assertEqual "remove node with left child only"
    (Node 10 "10" (Node 8 "8" (Node 1 "1" Leaf Leaf) (Node 9 "9" Leaf Leaf)) (Node 12 "12" Leaf (Node 14 "14" Leaf (Node 20 "20" Leaf Leaf))))
    (removeNode 7 fullTree)

removeNodeWithRightChildOnly :: HUnit.Assertion
removeNodeWithRightChildOnly = HUnit.assertEqual "remove node with right child only"
    (Node 10 "10" (Node 8 "8" (Node 7 "7" (Node 1 "1" Leaf Leaf) Leaf) (Node 9 "9" Leaf Leaf)) (Node 12 "12" Leaf (Node 20 "20" Leaf Leaf)))
    (removeNode 14 fullTree)

removeNodeWithTwoChildren :: HUnit.Assertion
removeNodeWithTwoChildren = HUnit.assertEqual "remove node with two children"
    (Node 10 "10" (Node 9 "9" (Node 7 "7" (Node 1 "1" Leaf Leaf) Leaf) Leaf) (Node 12 "12" Leaf (Node 14 "14" Leaf (Node 20 "20" Leaf Leaf))))
    (removeNode 8 fullTree)

removeNodeWithNoChildren :: HUnit.Assertion
removeNodeWithNoChildren = HUnit.assertEqual "remove node with no children"
    (Node 10 "10" (Node 8 "8" (Node 7 "7" (Node 1 "1" Leaf Leaf) Leaf) Leaf) (Node 12 "12" Leaf (Node 14 "14" Leaf (Node 20 "20" Leaf Leaf))))
    (removeNode 9 fullTree)

removeNodeThatDoesntExist :: HUnit.Assertion
removeNodeThatDoesntExist = HUnit.assertEqual "remove node that doesn't exist"
    (fullTree)
    (removeNode 100 fullTree)

-- removeIf tests
removeIfKeyGreaterThanTen :: HUnit.Assertion
removeIfKeyGreaterThanTen = HUnit.assertEqual "remove if key greater than ten"
    (Node 10 "10" (Node 8 "8" (Node 7 "7" (Node 1 "1" Leaf Leaf) Leaf) (Node 9 "9" Leaf Leaf)) Leaf)
    (removeIf (>10) fullTree)

removeIfKeyLessThanTwenty :: HUnit.Assertion
removeIfKeyLessThanTwenty = HUnit.assertEqual "remove if key less than twenty"
    (Node 20 "20" Leaf Leaf)
    (removeIf (<20) fullTree)
    
-- getListOfEntries tests
getAllEntriesOnEmptyTree :: HUnit.Assertion
getAllEntriesOnEmptyTree = HUnit.assertEqual "get all entries on empty tree"
    ([] :: [(Int, String)])
    (getListOfEntries Leaf)

getAllEntriesTest :: HUnit.Assertion
getAllEntriesTest = HUnit.assertEqual "get all entries"
    [(1,"1"),(7,"7"),(8,"8"),(9,"9"),(10,"10"),(12,"12"),(14,"14"),(20,"20")]
    (getListOfEntries fullTree)

getAllEntriesOnLeftSkewedTree :: HUnit.Assertion
getAllEntriesOnLeftSkewedTree = HUnit.assertEqual "get all entries on left skewed tree"
    [(1,"1"),(2,"2"),(4,"4"),(8,"8"),(10,"10")]
    (getListOfEntries leftSkewedTree)

getAllEntriesOnRightSkewedTree :: HUnit.Assertion
getAllEntriesOnRightSkewedTree = HUnit.assertEqual "get all entries on right skewed tree"
    [(10,"10"),(12,"12"),(14,"14"),(16,"16"),(18,"18"),(20,"20")]
    (getListOfEntries rightSkewedTree)

getAllEntriesOnSingleNodeTree :: HUnit.Assertion
getAllEntriesOnSingleNodeTree = HUnit.assertEqual "get all entries on single node tree"
    [(1,"1")]
    (getListOfEntries (createTreeFromList [(1,"1")]))

-- property tests
instance (Arbitrary keyType, Arbitrary valueType) => Arbitrary (BST keyType valueType) where 
    arbitrary = sized bst

bst :: (Arbitrary keyType, Arbitrary valueType) => Int -> Gen (BST keyType valueType)
bst size
    | size > 0 = do
        key <- arbitrary
        value <- arbitrary
        left <- bst (size `div` 2)
        right <- bst (size `div` 2)
        return (Node key value left right)
    | otherwise = return Leaf

treeSize :: BST key value -> Int
treeSize Leaf = 0
treeSize (Node _ _ left right) = 1 + (treeSize left) + (treeSize right)

prop_insertNodeIntString :: Int -> String -> BST Int String -> Bool
prop_insertNodeIntString key value tree = getValue key (insertNode key value tree) == Just value

prop_insertNodeStringString :: String -> String -> BST String String -> Bool
prop_insertNodeStringString key value tree = getValue key (insertNode key value tree) == Just value

prop_insertNodeIntInt :: Int -> Int -> BST Int Int -> Bool
prop_insertNodeIntInt key value tree = getValue key (insertNode key value tree) == Just value

prop_insertNodeCharDouble :: Char -> Double -> BST Char Double -> Bool
prop_insertNodeCharDouble key value tree = getValue key (insertNode key value tree) == Just value

prop_biggerTreeAfterInsert :: Int -> String -> BST Int String -> Bool
prop_biggerTreeAfterInsert key value tree = treeSize tree <= treeSize enlargedTree
    where enlargedTree = insertNode key value tree

prop_reducedTreeAfterRemoval :: Int -> String -> BST Int String -> Bool
prop_reducedTreeAfterRemoval key value tree = treeSize tree >= treeSize reducedTree
    where reducedTree = removeNode key tree

prop_reducedTreeAfterRemovalIf :: Int -> String -> BST Int String -> Bool
prop_reducedTreeAfterRemovalIf key value tree = treeSize tree >= treeSize reducedTree
    where reducedTree = removeIf (even) tree

tests = testGroup "bst tests" [
        testCase "create empty tree" createEmptyTreeTest,
        testCase "create node" createNodeTest,
        testCase "insert node into empty tree" insertNodeInEmptyTreeTest,
        testCase "insert new node" insertNewNode,
        testCase "insert string key/value" insertStringKeyAndValueTest,
        testCase "insert int key/value" insertIntKeyAndValueTest,
        testCase "insert different key/value" insertDifferentKeyAndValueTest,
        testCase "overwrite existing node value" insertNodeWithSameKeyTest,
        testCase "create left skewed tree" createLeftSkewedTreeTest,
        testCase "create right skewed tree" createRightSkewedTreeTest,
        testCase "create full tree" createFullTree,
        testCase "insert nodes from list" insertNodesFromListTest,
        testCase "create tree from list" createTreeFromListTest,
        testCase "get root value" getRootValue,
        testCase "get left most value" getLeftMostValue,
        testCase "get right most value" getRightMostValue,
        testCase "get value that doesnt exist" getValueThatDoesntExist,
        testCase "remove node with left child only" removeNodeWithLeftChildOnly,
        testCase "remove node with right child only" removeNodeWithRightChildOnly,
        testCase "remove node with two children" removeNodeWithTwoChildren,
        testCase "remove node with no children" removeNodeWithNoChildren,
        testCase "remove node that doesnt exist" removeNodeThatDoesntExist,
        testCase "remove if key greater than 10" removeIfKeyGreaterThanTen,
        testCase "remove if key less than 20" removeIfKeyLessThanTwenty,
        testCase "get all entries on empty tree" getAllEntriesOnEmptyTree,
        testCase "get all entries" getAllEntriesTest,
        testCase "get all entries on left skewed tree" getAllEntriesOnLeftSkewedTree,
        testCase "get all entries on right skewed tree" getAllEntriesOnRightSkewedTree,
        testCase "get all entries on single node tree" getAllEntriesOnSingleNodeTree,
        testProperty "insert valid for key int and value string" prop_insertNodeIntString,
        testProperty "insert valid for key string and value string" prop_insertNodeStringString,
        testProperty "insert valid for key int and value int" prop_insertNodeIntInt,
        testProperty "insert valid for key char and value double" prop_insertNodeCharDouble,
        testProperty "tree bigger after node inserted" prop_biggerTreeAfterInsert,
        testProperty "tree reduced after node removed" prop_reducedTreeAfterRemoval,
        testProperty "remove if bst check" prop_reducedTreeAfterRemovalIf
    ]