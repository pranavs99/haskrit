module Phonology.Sandhi.Types where

import Phonology.Phoneme.Types
    (Phoneme(..))


{-
    the Phoneme-level result of a sandhi application attempt
-}
data BoundarySandhiResult
    = SandhifiedPhonemes [Phoneme]
    | UnchangedPhonemes (Phoneme, Phoneme)
    deriving (Show)

{-
    the word-level result of a sandhi application attempt
-}
data SandhiResult
    = Sandhita String
    | UnchangedWords (String, String)
    deriving (Show, Eq)
