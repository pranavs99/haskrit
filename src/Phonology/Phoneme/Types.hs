module Phonology.Phoneme.Types where

import Phonology.Feature


data Phoneme
    = Vowel String [Feature]
    | Consonant String [Feature]
    | Unrecognized String
    deriving (Show, Eq)
