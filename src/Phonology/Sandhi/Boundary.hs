module Phonology.Sandhi.Boundary where

import Phonology.Phoneme.Types
import Phonology.Phoneme
import Phonology.Sandhi.Vocalic as Vocalic
import Phonology.Sandhi.Consonantal as Consonantal
import Phonology.Sandhi.Types
import Phonology.Utilities as Utils


{-
    apply sandhi based on boundary

    inputs: - trace as String
            - (rem, left) (right, rem)
    output: boundary sandhi attempt
-}
applySandhiByBoundary
    :: String
    -> ([Phoneme], Phoneme) -> (Phoneme, [Phoneme])
    -> BoundarySandhiResult
applySandhiByBoundary trace (leftRem, left) (right, rightRem)

    -- "ac" (vowel) sandhi
    | inSutra left "a" "c" && inSutra right "a" "c" =
        Vocalic.applyBoundaryAc (trace ++ "/V") (leftRem, left) (right, rightRem)

    -- "hal" (consonant) sandhi
    | inSutra left "h1" "l" =
        Consonantal.applyBoundaryHal (trace ++ "/C") (leftRem, left) (right, rightRem)

    | otherwise =
        UnchangedPhonemes (left, right)

applyBoundarySandhiOnPhonemes
    :: String                   -- trace
    -> ([Phoneme], [Phoneme])   -- (left phonemes, right phonemes)
    -> BoundarySandhiResult     -- boundary sandhi result
applyBoundarySandhiOnPhonemes trace (leftPhonemes, rightPhonemes) =
    let
        (leftRemainder, leftBoundary)   = Utils.splitLast leftPhonemes
        (rightBoundary, rightRemainder) = Utils.splitFirst rightPhonemes
    in
        applySandhiByBoundary
            trace (leftRemainder, leftBoundary) (rightBoundary, rightRemainder)

{-
    extract sandhi contexts and stitch successful sandhi application results
    into a Sandhita constructor

    input: two contiguous words (can also be previous sandhitas) as Strings
    output: either a successful sandhita, or a tuple containing the unchanged
            input words

    * adequate start position for post-word boundary sandhi *
-}
applyBoundarySandhi :: (String, String) -> SandhiResult
applyBoundarySandhi (leftWord, rightWord) =
    let
        leftPhonemes    = phonemesOf leftWord
        rightPhonemes   = phonemesOf rightWord
    in
        -- start boundary sandhi off with trace "word"
        case applyBoundarySandhiOnPhonemes "word" (leftPhonemes, rightPhonemes) of
            MergedBoundary (sandhLeftRem, sandhified, sandhRightRem) trace ->
                Sandhita
                    (concatMap soundOf sandhLeftRem
                        ++ concatMap soundOf sandhified
                        ++ concatMap soundOf sandhRightRem)
                    trace
            -- to keep retained boundary sandhi flexible, either the left
            -- or right context phoneme can be elided by returning Nothing
            RetainedBoundary (sandhLeftRem, Nothing) (Nothing, sandhRightRem) trace ->
                Sandhita
                    (concatMap soundOf sandhLeftRem
                        ++ " "
                        ++ concatMap soundOf sandhRightRem)
                    trace
            RetainedBoundary (sandhLeftRem, Just left) (Nothing, sandhRightRem) trace ->
                Sandhita
                    (concatMap soundOf sandhLeftRem
                        ++ soundOf left
                        ++ " "
                        ++ concatMap soundOf sandhRightRem)
                    trace
            RetainedBoundary (sandhLeftRem, Nothing) (Just right, sandhRightRem) trace ->
                Sandhita
                    (concatMap soundOf sandhLeftRem
                        ++ " "
                        ++ soundOf right
                        ++ concatMap soundOf sandhRightRem)
                    trace
            RetainedBoundary (sandhLeftRem, Just left) (Just right, sandhRightRem) trace ->
                Sandhita
                    (concatMap soundOf sandhLeftRem
                        ++ soundOf left
                        ++ " "
                        ++ soundOf right
                        ++ concatMap soundOf sandhRightRem)
                    trace

            UnchangedPhonemes _ -> Unchanged (leftWord, rightWord)
