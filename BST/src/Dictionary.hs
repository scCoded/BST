module Dictionary where

import BST

-- Encapsulate BST within Dictionary
data Dictionary keyType valueType = Dictionary (BST keyType valueType) deriving (Show, Read, Eq)

-- Create new dictionary
newDictionary :: Dictionary keyType valueType
newDictionary = Dictionary createEmptyTree

-- Add item to dictionary
-- addToDictionary :: (Ord keyType) => Dictionary keyType valueType -> keyType -> valueType -> Dictionary keyType valueType
-- addToDictionary (Dictionary tree) key value = Dictionary (insertNode key value tree )

addToDict :: (Ord keyType, Eq keyType) => keyType -> valueType -> Dictionary keyType valueType -> Dictionary keyType valueType
addToDict key value (Dictionary tree) = Dictionary (insertNode key value tree)

-- Create dict from List
createDictionaryFromList :: (Ord keyType, Eq keyType) => [(keyType, valueType)] -> Dictionary keyType valueType
createDictionaryFromList list = Dictionary (createTreeFromList list)

-- Remove item from dictionary
removeFromDict :: (Ord keyType, Eq keyType) => keyType -> Dictionary keyType valueType -> Dictionary keyType valueType
removeFromDict key (Dictionary tree) = Dictionary (removeNode key tree)

-- RemoveIf items from dictionary
removeIfFromDict :: (Ord keyType, Eq keyType) => (keyType -> Bool) -> Dictionary keyType valueType -> Dictionary keyType valueType
removeIfFromDict predicate (Dictionary tree) = Dictionary (removeIf predicate tree)

-- Get value from dictionary
getValueFromDict :: (Ord keyType, Eq keyType) => keyType -> Dictionary keyType valueType -> Maybe valueType
getValueFromDict key (Dictionary tree) = getValue key tree

-- Get all entries
getAllEntries :: (Ord keyType, Eq keyType) => Dictionary keyType valueType -> [(keyType, valueType)]
getAllEntries (Dictionary tree) = getListOfEntries tree