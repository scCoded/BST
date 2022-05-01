module Dictionary_Tests where

import Test.HUnit as HUnit
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Dictionary
import BST

-- insertNode() unit tests
insertValueIntoEmptyDictionary :: HUnit.Assertion
insertValueIntoEmptyDictionary = HUnit.assertEqual "insert value into empty dictionary"
    (Dictionary (BST.Node 1 1 BST.Leaf BST.Leaf))
    (addToDict 1 1 newDictionary)

tests = testGroup "dictionary tests" [
    testCase "test1" insertValueIntoEmptyDictionary
    ]