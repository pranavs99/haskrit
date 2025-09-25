module Phonology.Utilities where

import Data.List


errorMessages :: [String] -> a
errorMessages = error . unlines

reversePair :: Eq a => (a, a) -> (a, a)
reversePair (x, y) = (y, x)

pairMatches :: Eq a => (a, a) -> (a, a) -> Bool
pairMatches given against =
    given == against || given == reversePair against

hasPair :: Eq a => [(a, a)] -> (a, a) -> Bool
hasPair [] _ = False
hasPair (against : rest) given =
    pairMatches given against || hasPair rest given

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf subset superset = all (`elem` superset) subset

splitFirst :: [a] -> (a, [a])
splitFirst []           = error "tried to call splitFirst on empty list"
splitFirst (x : rest)   = (x, rest)

splitLast :: [a] -> ([a], a)
splitLast [] = error "tried to call splitLast on empty list"
splitLast xs = (init xs, last xs)

splitAt :: Eq a => a -> [a] -> ([a], [a])
splitAt at xs = split at xs [] where
    split :: Eq a => a -> [a] -> [a] -> ([a], [a])
    split _ [] acc =
        (reverse acc, [])
    split target (curr : rest) acc
        | curr == target    = (reverse acc, curr : rest)
        | otherwise         = split target rest (curr : acc)

printStrings :: [String] -> IO ()
printStrings strs = putStrLn $ Data.List.intercalate ", " strs

predicateMatches :: Eq b => (a -> b) -> a -> a -> Bool
predicateMatches f first second =
    f first == f second

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove target (curr : rest)
    | target == curr    = remove target rest
    | otherwise         = curr : remove target rest

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll _ [] = []
removeAll [] against = against
removeAll (currTarget : restTargets) against =
    removeAll restTargets (remove currTarget against)
