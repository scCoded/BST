module BST where

data BST keyType valueType = Node keyType valueType (BST keyType valueType) (BST keyType valueType)
        | Leaf deriving (Show, Read, Eq)

createEmptyTree :: (Eq keyType, Eq keyType) => (BST keyType valueType)
createEmptyTree = Leaf

createNode :: (Ord keyType, Eq keyType) => keyType -> valueType -> BST keyType valueType
createNode key value = Node key value Leaf Leaf

insertNode :: (Ord keyType, Eq keyType) => keyType -> valueType -> BST keyType valueType -> BST keyType valueType
insertNode key value Leaf = createNode key value
insertNode key value (Node nextKey nextValue left right)
        | key < nextKey = Node nextKey nextValue (insertNode key value left) right
        | key > nextKey = Node nextKey nextValue left (insertNode key value right)
        | otherwise = Node key value left right

insertNodesFromList :: (Ord keyType, Eq keyType) => [(keyType, valueType)] -> BST keyType valueType -> BST keyType valueType
insertNodesFromList [] tree = tree
insertNodesFromList (x:xs) tree = insertNodesFromList xs (insertNode (fst x) (snd x) tree)

createTreeFromList :: (Ord keyType, Eq keyType) => [(keyType, valueType)] -> BST keyType valueType
createTreeFromList list = insertNodesFromList list Leaf

getValue :: (Ord keyType, Eq keyType) => keyType -> BST keyType valueType -> Maybe valueType
getValue _ Leaf = Nothing
getValue currentKey (Node nextKey value left right)
        | currentKey == nextKey = Just value
        | currentKey < nextKey = getValue currentKey left
        | otherwise = getValue currentKey right

removeNode :: (Ord keyType, Eq keyType) => keyType -> BST keyType valueType -> BST keyType valueType
removeNode _ Leaf = Leaf
removeNode indexToRemove (Node key value left right)
        | indexToRemove < key = Node key value (removeNode indexToRemove left) right
        | indexToRemove > key = Node key value left (removeNode indexToRemove right)
        | otherwise = removeNode' (Node key value left right)

removeNode' :: (Ord keyType, Eq keyType) => BST keyType valueType -> BST keyType valueType
removeNode' Leaf = Leaf
removeNode' (Node key value Leaf right) = right
removeNode' (Node key value left Leaf) = left
removeNode' (Node key value left right) = (Node minNodeKey minValue left newRight)
                    where
                        (minNodeKey, minValue) = detachMinimumNode right
                        newRight = removeNode minNodeKey right

detachMinimumNode :: (Ord keyType, Eq keyType) => BST keyType valueType -> (keyType, valueType)
detachMinimumNode (Node key value Leaf _) = (key, value)
detachMinimumNode (Node key _ left _) = detachMinimumNode left

removeIf :: (Ord keyType, Eq keyType) => (keyType -> Bool) -> BST keyType valueType -> BST keyType valueType
removeIf _ Leaf = Leaf
removeIf predicate (Node key value left right)
        | predicate key = removeNode key (Node key value (removeIf predicate left) (removeIf predicate right))
        | otherwise = Node key value (removeIf predicate left) (removeIf predicate right)

getListOfEntries :: (Ord keyType, Eq keyType) => BST keyType valueType -> [(keyType, valueType)]
getListOfEntries Leaf = []
getListOfEntries (Node key value left right) = (getListOfEntries left) ++ [(key, value)] ++ (getListOfEntries right)