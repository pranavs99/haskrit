module Phonology.Phoneme.Types where

import Phonology.Feature


data Phoneme
    = Vowel String [Feature]
    | Consonant String [Feature]
    deriving (Show, Eq)
