module Lib where

data BST valueType = Node Int valueType (BST valueType) (BST valueType)
        | Leaf deriving (Show, Read, Eq)

createEmptyTree :: BST valueType
createEmptyTree = Leaf

createNode :: Int -> valueType -> BST valueType
createNode key value = Node key value Leaf Leaf

insertNode :: Int -> valueType -> BST valueType -> BST valueType
insertNode key value Leaf = createNode key value
insertNode key value (Node nextKey nextValue left right)
        | key < nextKey = Node nextKey nextValue (insertNode key value left) right
        | key > nextKey = Node nextKey nextValue left (insertNode key value right)
        | otherwise = Node key value left right