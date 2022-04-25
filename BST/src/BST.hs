module BST where

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

removeNode :: Int -> BST valueType -> BST valueType
removeNode _ Leaf = error "Key not found"
removeNode indexToRemove (Node key value left right)
        | indexToRemove < key = Node key value (removeNode indexToRemove left) right
        | indexToRemove > key = Node key value left (removeNode indexToRemove right)
        | otherwise = removeNode' (Node key value left right)

removeNode' :: BST valueType -> BST valueType
removeNode' Leaf = Leaf
removeNode' (Node key value Leaf right) = right
removeNode' (Node key value left Leaf) = left
removeNode' (Node key value left right) = (Node minNodeKey minValue left newRight)
                    where
                        minNodeKey = detachMinimumNode right
                        minValue = getValue minNodeKey right
                        newRight = removeNode minNodeKey right

detachMinimumNode :: BST valueType -> Int
detachMinimumNode Leaf = error "Empty tree"
detachMinimumNode (Node key _ Leaf _) = key
detachMinimumNode (Node key _ left _) = detachMinimumNode left

removeIf :: (Int -> Bool) -> BST valueType -> BST valueType
removeIf _ Leaf = Leaf
removeIf predicate (Node key value left right)
        | predicate key = removeNode key (Node key value (removeIf predicate left) (removeIf predicate right))
        | otherwise = Node key value (removeIf predicate left) (removeIf predicate right)

getListOfEntries :: BST valueType -> [(Int, valueType)]
getListOfEntries Leaf = []
getListOfEntries (Node key value left right) = (getListOfEntries left) ++ [(key, value)] ++ (getListOfEntries right)