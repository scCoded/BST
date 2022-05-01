module Dictionary_Tests where

import Test.HUnit
import Test.QuickCheck
import Dictionary
import BST

-- insertNode() unit tests
insertValueIntoEmptyDictionary :: Test
insertValueIntoEmptyDictionary = TestCase (assertEqual "insert value into empty dictionary"
    (Dictionary (BST.Node 1 1 BST.Leaf BST.Leaf))
    (addToDict 1 1 newDictionary)
    )


tests = [
    insertValueIntoEmptyDictionary
    ]