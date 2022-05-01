import BST_Tests
import Dictionary_Tests

import System.Exit
import Test.HUnit

main :: IO ()
main = do
    results <- runTestTT $
                    test (BST_Tests.tests ++ Dictionary_Tests.tests)
    if errors results + failures results == 0 then
        putStrLn "Tests passed."
    else
        die "Tests failed."