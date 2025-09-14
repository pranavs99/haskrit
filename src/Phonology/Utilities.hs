module Phonology.Utilities where


reversePair :: Eq a => (a, a) -> (a, a)
reversePair (x, y) = (y, x)

pairMatches :: Eq a => (a, a) -> (a, a) -> Bool
pairMatches given against =
    given == against || given == reversePair (against)

hasPair :: Eq a => [(a, a)] -> (a, a) -> Bool
hasPair [] _ = False
hasPair (against : rest) given =
    pairMatches given against || hasPair rest given

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf subset superset = all (`elem` superset) subset

splitFirst :: [a] -> (a, [a])
splitFirst []           = error ("tried to call splitFirst on empty list")
splitFirst (x : rest)   = (x, rest)

splitLast :: [a] -> ([a], a)
splitLast [] = error ("tried to call splitLast on empty list")
splitLast xs = (init xs, last xs)
