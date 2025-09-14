module Phonology.Phoneme.Types where

import Phonology.Feature

{-
    avoiding a cyclic dependency

    problem:
    Phoneme.hs needs to refer to Phoneme/Inventory.hs to write valid phoneme
    parsing code, but Phoneme/Inventory.hs needs to refer to the Phoneme type,
    which should ideally be defined in Phoneme.hs, causing a cyclic dependency.

    solution:
    import the phoneme inventory from Phoneme/Inventory.hs into Phoneme.hs
    to keep higher-level parsing logic in Phoneme.hs, and move the contentious
    Phoneme type to this module so that both Phoneme.hs and Phoneme/Inventory.hs
    can import the Phoneme type safely.
-}

data Phoneme
    = Vowel String [Feature]
    | Consonant String [Feature]
    deriving (Show, Eq)
