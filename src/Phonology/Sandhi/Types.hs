module Phonology.Sandhi.Types where

import Phonology.Phoneme.Types
    (Phoneme(..))


{-
    the Phoneme-level result of a sandhi application attempt
    SandhifiedPhonemes [fused phonemes] "reason"
    UnchangedPhonemes (original context) "attempted"
-}
data BoundarySandhiResult
    = MergedBoundary    ([Phoneme], [Phoneme], [Phoneme]) String
    | RetainedBoundary  ([Phoneme], Maybe Phoneme) (Maybe Phoneme, [Phoneme]) String
    | UnchangedPhonemes (Phoneme, Phoneme)
    deriving (Show)

{-
    the word-level result of a sandhi application attempt
-}
data SandhiResult
    = Sandhita      String String
    | Unchanged     (String, String)
    deriving (Show, Eq)
