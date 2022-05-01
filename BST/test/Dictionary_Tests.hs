module Dictionary_Tests where

import Test.HUnit as HUnit
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Dictionary
import BST

-- newDictionary tests
createEmptyDict :: HUnit.Assertion
createEmptyDict = HUnit.assertEqual "insert value into empty dictionary"
    (Dictionary (BST.Leaf :: BST Int Int)) 
    (newDictionary)

-- insertValue tests
insertValueIntoEmptyDictionary :: HUnit.Assertion
insertValueIntoEmptyDictionary = HUnit.assertEqual "insert value into empty dictionary"
    (Dictionary (BST.Node 1 1 BST.Leaf BST.Leaf))
    (addToDict 1 1 newDictionary)

insertNewValue :: HUnit.Assertion
dict = createDictionaryFromList[(7,"7"), (8,"8"), (9,"9"), (10,"10")]
insertNewValue = HUnit.assertEqual "insert new value"
    (Dictionary (Node 7 "7" (Node 2 "newValue" Leaf Leaf) (Node 8 "8" Leaf (Node 9 "9" Leaf (Node 10 "10" Leaf Leaf)))))
    (addToDict 2 "newValue" dict)

insertStringKeyAndValueTest :: HUnit.Assertion
insertStringKeyAndValueTest = HUnit.assertEqual "insert string key/value type"
    (Dictionary (Node "a" "a" Leaf Leaf))
    (addToDict "a" "a" newDictionary)

insertIntKeyAndValueTest :: HUnit.Assertion
insertIntKeyAndValueTest = HUnit.assertEqual "insert int key/value type"
    (Dictionary (Node 1 1 Leaf Leaf))
    (addToDict 1 1 newDictionary)

insertDifferentKeyAndValueTest :: HUnit.Assertion
insertDifferentKeyAndValueTest = HUnit.assertEqual "insert different key/value type"
    (Dictionary (Node 1 "1" Leaf Leaf))
    (addToDict 1 "1" newDictionary)

insertValueWithExistingKey :: HUnit.Assertion
insertValueWithExistingKey = HUnit.assertEqual "insert value with existing key"
    (Dictionary (Node 7 "updated" Leaf (Node 8 "8" Leaf (Node 9 "9" Leaf (Node 10 "10" Leaf Leaf)))))
    (addToDict 7 "updated" dict)

createLeftSkewedDict :: HUnit.Assertion
leftSkewedDict = createDictionaryFromList [(10, "10"), (8, "8"), (4, "4"), (2, "2"), (1, "1")]
createLeftSkewedDict = HUnit.assertEqual "create left skewed dict"
    (Dictionary (Node 10 "10" (Node 8 "8" (Node 4 "4" (Node 2 "2" (Node 1 "1" Leaf Leaf) Leaf) Leaf) Leaf) Leaf))
    leftSkewedDict

createRightSkewedDict :: HUnit.Assertion
rightSkewedDict = createDictionaryFromList [(10, "10"), (12, "12"), (14, "14"), (16, "16"), (18, "18"), (20, "20")]
createRightSkewedDict = HUnit.assertEqual "create right skewed dict, by inserting increasingly larger keys"
    (Dictionary (Node 10 "10" Leaf (Node 12 "12" Leaf (Node 14 "14" Leaf (Node 16 "16" Leaf (Node 18 "18" Leaf (Node 20 "20" Leaf Leaf)))))))
    rightSkewedDict

createFullDict :: HUnit.Assertion
fullDict = createDictionaryFromList [(10, "10"), (12, "12"), (8, "8"), (7, "7"), (14, "14"), (9, "9"), (20, "20"), (1, "1")]
createFullDict = HUnit.assertEqual "create full dict"
    (Dictionary (Node 10 "10" (Node 8 "8" (Node 7 "7" (Node 1 "1" Leaf Leaf) Leaf) (Node 9 "9" Leaf Leaf)) (Node 12 "12" Leaf (Node 14 "14" Leaf (Node 20 "20" Leaf Leaf)))))
    fullDict

-- create dictionary from list tests
createDictFromList :: HUnit.Assertion
createDictFromList = HUnit.assertEqual "create dictionary from list"
    (Dictionary (Node 10 "10" (Node 8 "8" (Node 7 "7" (Node 1 "1" Leaf Leaf) Leaf) (Node 9 "9" Leaf Leaf)) (Node 12 "12" Leaf (Node 14 "14" Leaf (Node 20 "20" Leaf Leaf)))))
    (createDictionaryFromList [(10, "10"), (8, "8"), (7, "7"), (1, "1"), (9, "9"), (12, "12"), (14, "14"), (20, "20")])

-- get value
getValueFromDictTest :: HUnit.Assertion
getValueFromDictTest = HUnit.assertEqual "get value from dictionary"
    (Just "10")
    (getValueFromDict 10 dict)

getLeftMostValue :: HUnit.Assertion
getLeftMostValue = HUnit.assertEqual "get left most value from dictionary"
    (Just "1")
    (getValueFromDict 1 fullDict)

getRightMostValue :: HUnit.Assertion
getRightMostValue = HUnit.assertEqual "get right most value from dictionary"
    (Just "20")
    (getValueFromDict 20 fullDict)

getValueThatDoesntExist :: HUnit.Assertion
getValueThatDoesntExist = HUnit.assertEqual "get value that doesn't exist"
    Nothing
    (getValueFromDict 100 dict)

-- removeValue
removeValueWithLeftChildOnly :: HUnit.Assertion
removeValueWithLeftChildOnly = HUnit.assertEqual "remove value with left child"
    (Dictionary (Node 10 "10" (Node 8 "8" (Node 1 "1" Leaf Leaf) (Node 9 "9" Leaf Leaf)) (Node 12 "12" Leaf (Node 14 "14" Leaf (Node 20 "20" Leaf Leaf)))))
    (removeFromDict 7 fullDict)

removeValueWithRightChildOnly :: HUnit.Assertion
removeValueWithRightChildOnly = HUnit.assertEqual "remove value with right child"
    (Dictionary (Node 10 "10" (Node 8 "8" (Node 7 "7" (Node 1 "1" Leaf Leaf) Leaf) (Node 9 "9" Leaf Leaf)) (Node 12 "12" Leaf (Node 20 "20" Leaf Leaf))))
    (removeFromDict 14 fullDict)

removeValueWithTwoChildren :: HUnit.Assertion
removeValueWithTwoChildren = HUnit.assertEqual "remove value with two children"
    (Dictionary (Node 10 "10" (Node 9 "9" (Node 7 "7" (Node 1 "1" Leaf Leaf) Leaf) Leaf) (Node 12 "12" Leaf (Node 14 "14" Leaf (Node 20 "20" Leaf Leaf)))))
    (removeFromDict 8 fullDict)

removeValueWithNoChildren :: HUnit.Assertion
removeValueWithNoChildren = HUnit.assertEqual "remove value with no children"
    (Dictionary (Node 10 "10" (Node 8 "8" (Node 7 "7" (Node 1 "1" Leaf Leaf) Leaf) Leaf) (Node 12 "12" Leaf (Node 14 "14" Leaf (Node 20 "20" Leaf Leaf)))))
    (removeFromDict 9 fullDict)

removeValueThatDoesntExist :: HUnit.Assertion
removeValueThatDoesntExist = HUnit.assertEqual "remove value that doesn't exist"
    (Dictionary (Node 10 "10" (Node 8 "8" (Node 7 "7" (Node 1 "1" Leaf Leaf) Leaf) (Node 9 "9" Leaf Leaf)) (Node 12 "12" Leaf (Node 14 "14" Leaf (Node 20 "20" Leaf Leaf)))))
    (removeFromDict 100 fullDict)

-- removeIf tests
removeIfKeyGreaterThanTen :: HUnit.Assertion
removeIfKeyGreaterThanTen = HUnit.assertEqual "remove if key greater than 10"
    (Dictionary (Node 10 "10" (Node 8 "8" (Node 7 "7" (Node 1 "1" Leaf Leaf) Leaf) (Node 9 "9" Leaf Leaf)) Leaf))
    (removeIfFromDict (>10) fullDict)

removeIfKeyLessThan20 :: HUnit.Assertion
removeIfKeyLessThan20 = HUnit.assertEqual "remove if key less than 20"
    (Dictionary (Node 20 "20" Leaf Leaf))
    (removeIfFromDict (<20) fullDict)

-- getAllEntries tests
getAllEntriesTest :: HUnit.Assertion
getAllEntriesTest = HUnit.assertEqual "get all entries"
    [(1,"1"),(7,"7"),(8,"8"),(9,"9"),(10,"10"),(12,"12"),(14,"14"),(20,"20")]
    (getAllEntries fullDict)

getAllEntriesOnRightSkewedDict :: HUnit.Assertion
getAllEntriesOnRightSkewedDict = HUnit.assertEqual "get all entries on right skewed tree"
    [(10,"10"),(12,"12"),(14,"14"),(16,"16"),(18,"18"),(20,"20")]
    (getAllEntries rightSkewedDict)

getAllEntriesOnLeftSkewedDict :: HUnit.Assertion
getAllEntriesOnLeftSkewedDict = HUnit.assertEqual "get all entries on left skewed tree"
    [(1,"1"),(2,"2"),(4,"4"),(8,"8"),(10,"10")]
    (getAllEntries leftSkewedDict)

getAllEntriesOnSingleValueDict :: HUnit.Assertion
getAllEntriesOnSingleValueDict = HUnit.assertEqual "get all entries on single value tree"
    [(10, "10")]
    (getAllEntries (createDictionaryFromList [(10, "10")]))

tests = testGroup "dictionary tests" [
    testCase "create an empty dictionary" createEmptyDict,
    testCase "insert a value into an empty dictionary" insertValueIntoEmptyDictionary,
    testCase "insert new value into existing dict" insertNewValue,
    testCase "insert string key/value" insertStringKeyAndValueTest,
    testCase "insert int key/value" insertIntKeyAndValueTest,
    testCase "insert different key/value" insertDifferentKeyAndValueTest,
    testCase "insert value with existing key" insertValueWithExistingKey,
    testCase "create left skewed dict" createLeftSkewedDict,
    testCase "create right skewed dict" createRightSkewedDict,
    testCase "create full dict" createFullDict,
    testCase "create dictionary from list" createDictFromList,
    testCase "get value from dictionary" getValueFromDictTest,
    testCase "get left most value from dictionary" getLeftMostValue,
    testCase "get right most value from dictionary" getRightMostValue,
    testCase "get value that doesn't exist" getValueThatDoesntExist,
    testCase "remove value with left child only" removeValueWithLeftChildOnly,
    testCase "remove value with right child only" removeValueWithRightChildOnly,
    testCase "remove value with two children" removeValueWithTwoChildren,
    testCase "remove value with no children" removeValueWithNoChildren,
    testCase "remove value that doesn't exist" removeValueThatDoesntExist,
    testCase "remove if key greater than 10" removeIfKeyGreaterThanTen,
    testCase "remove if key less than 20" removeIfKeyLessThan20,
    testCase "get all entries" getAllEntriesTest,
    testCase "get all entries on right skewed dict" getAllEntriesOnRightSkewedDict,
    testCase "get all entries on left skewed dict" getAllEntriesOnLeftSkewedDict,
    testCase "get all entries on single value dict" getAllEntriesOnSingleValueDict
    ]