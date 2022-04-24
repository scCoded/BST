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

getValue :: Int -> BST valueType -> valueType
getValue _ Leaf = error "Key not found"
getValue currentKey (Node nextKey value left right)
        | currentKey == nextKey = value
        | currentKey < nextKey = getValue currentKey left
        | otherwise = getValue currentKey right

getListOfEntries :: BST valueType -> [(Int, valueType)]
getListOfEntries Leaf = []
getListOfEntries (Node key value left right) = (getListOfEntries left) ++ [(key, value)] ++ (getListOfEntries right)