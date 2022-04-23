module Lib where
import Data.IntMap (insert)

data BST = Node Int String BST BST
        | Leaf
    -- deriving (Eq, Show)

insertNode:: Int -> String -> BST
insertNode key value = undefined

lookupByKey:: Int -> BST
lookupByKey key = undefined

showTree:: BST -> IO ()
showTree = undefined

removeByKey:: Int-> BST
removeByKey x = undefined

removeByPredicate:: [a] -> (a -> Bool) -> [a]
removeByPredicate = undefined