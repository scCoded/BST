import BST_Tests
import Dictionary_Tests

import System.Exit
import Test.HUnit
import Test.Tasty

tests :: TestTree
tests = testGroup "Tests" [BST_Tests.tests, Dictionary_Tests.tests]

main :: IO ()
main = defaultMain Main.tests
