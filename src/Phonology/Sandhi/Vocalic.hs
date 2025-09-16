module Phonology.Sandhi.Vocalic where

import Phonology.Phoneme
import Phonology.Phoneme.Types
import Phonology.Phoneme.Sutra
import Phonology.Phoneme.Alternation
import Phonology.Phoneme.Feature
import Phonology.Sandhi.Types


{-
    identify and apply vowel-only sandhi rules

    input: left and right vowel contexts as Phonemes
    output: either a successful vowel sandhi application,
            or a tuple containing the unchanged vowels
-}
applyVocalicSandhi :: Phoneme -> Phoneme -> BoundarySandhiResult
applyVocalicSandhi leftVowel rightVowel
    | qualitiesMatch leftVowel rightVowel =
        applySavarNa leftVowel rightVowel
    | otherwise =
        applySaMhita leftVowel rightVowel

{-
    सवर्ण "savarNa"
    def) two vowels with the same quality coalesce to a long vowel of the same
         quality
    --> only the qualities "a", "i", and "u" can really exhibit this,
        so qualityOf can only match with those vowels
-}
applySavarNa :: Phoneme -> Phoneme -> BoundarySandhiResult
applySavarNa leftVowel rightVowel = case elongate leftVowel of
    Just elongated  -> SandhifiedPhonemes [elongated]
    Nothing         -> UnchangedPhonemes (leftVowel, rightVowel)

{-
    संहित "saMhita" ("close contact")
    def) when two vowels share a boundary (in "close contact"), the first vowel
         transforms to a semivowel matching Place
-}
applySaMhita :: Phoneme -> Phoneme -> BoundarySandhiResult
applySaMhita leftVowel rightVowel
    | soundOf leftVowel `elem` matchSutra "i" "k" =
        case changeManner Approximant (featuresOf leftVowel) of
            Just changedFeatures -> case matchPhoneme changedFeatures of
                Just matchedApproximant ->
                    SandhifiedPhonemes [matchedApproximant, rightVowel]
                Nothing ->
                    UnchangedPhonemes (leftVowel, rightVowel)
            _ ->
                UnchangedPhonemes (leftVowel, rightVowel)
    | otherwise =
        UnchangedPhonemes (leftVowel, rightVowel)
