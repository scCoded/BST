module Dictionary where

import BST

-- Encapsulate BST within Dictionary
-- data Dictionary a = Dictionary (BST a)
newtype Dictionary valueType = Dictionary (BST valueType) deriving (Show)

-- Create new dictionary
newDictionary :: Dictionary valueType
newDictionary = Dictionary createEmptyTree

-- Add item to dictionary
addToDict :: Int -> valueType -> Dictionary valueType -> Dictionary valueType
addToDict key value (Dictionary tree) = Dictionary (insertNode key value tree)

-- Remove item from dictionary
removeFromDict :: Int -> Dictionary valueType -> Dictionary valueType
removeFromDict key (Dictionary tree) = Dictionary (removeNode key tree)

-- RemoveIf items from dictionary
removeIfFromDict :: (Int -> Bool) -> Dictionary valueType -> Dictionary valueType
removeIfFromDict predicate (Dictionary tree) = Dictionary (removeIf predicate tree)

-- Get value from dictionary
getValueFromDict :: Int -> Dictionary valueType -> valueType
getValueFromDict key (Dictionary tree) = getValue key tree

-- Get all entries
getAllEntries :: Dictionary valueType -> [(Int, valueType)]
getAllEntries (Dictionary tree) = getListOfEntries tree

