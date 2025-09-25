module Phonology.Sandhi.Vocalic where

import Phonology.Phoneme
import Phonology.Phoneme.Types
import Phonology.Phoneme.Alternation
import Phonology.Sandhi.Types

{-
    apply vowel (a -> c) sandhi

    inputs: - trace (String)
            - (rem, left) (right, rem)
    output: SandhiResult
-}
applyBoundaryAc
    :: String
    -> ([Phoneme], Phoneme) -> (Phoneme, [Phoneme])
    -> BoundarySandhiResult
applyBoundaryAc
    trace (leftRemainder, left) (right, rightRemainder)

    -- 6.1.101 "aka-H savarN-e di:rgha-H"
    | inSutra left "a" "k" && qualitiesMatch left right =
        savarNa trace (leftRemainder, left) (right, rightRemainder)

    -- saMhita entrypoint
    | otherwise =
        saMhita (trace ++ "/saMhita") (leftRemainder, left) (right, rightRemainder)

{-  6.1.101
    "aka-H  savarN-e    di:rgha-H"
    {ak}    like vowel  become long
    def) two vowels with like quality are replaced by one elongated version

    --> guarantee:  like "ak" vowels
    --> curries:    String label (3rd argument)
-}
savarNa
    :: String
    -> ([Phoneme], Phoneme) -> (Phoneme, [Phoneme])
    -> BoundarySandhiResult
savarNa trace (leftRem, left) (_, rightRem) =

    MergedBoundary
        (leftRem, [elongate left], rightRem)
        (trace ++ "/savarNa [6.1.101]")

{-  1.4.109: संहित saMhita
    "para-H     saMnikarSa-H    saMhita:"
    very        close contact   is SaMhita
    def) when two vowels share a boundary (in "close contact"), the first vowel
         transforms to a semivowel matching Place

    --> guarantee: unlike "ak" vowels at boundar
-}
saMhita
    :: String
    -> ([Phoneme], Phoneme) -> (Phoneme, [Phoneme])
    -> BoundarySandhiResult
saMhita trace (leftRem, left) (right, rightRem)

    -- 6.1.77 "ika-H yaN aci" -> "ik-o yaNaci"
    | inSutra left "i" "k" =
        MergedBoundary
            (leftRem, [approximantize left, right], rightRem)
            (trace ++ ".L-approx [6.1.77]")

    -- 6.1.87 "a:t guNa-H" -> "a:dguNaH"
    {-  6.1.87
        a:t guNa-H

        ... a(:) | i(:),u(:) ...
        ...
    -}
    | qualityOf left == "a" && inSutra right "i" "k" =
        let aTrace = (trace ++ ".L-a") in
        if inSutra right "i" "N1" then
            MergedBoundary
                (leftRem, [compositize right], rightRem)
                (aTrace ++ ".R-comp [6.1.87]")
        else
            MergedBoundary
                (leftRem, [confirmPhoneme "a", approximantize right], rightRem)
                (aTrace ++ ".R-approx [6.1.87]")

    -- 6.1.88 "vRddhi-H eci" -> "vRddhireci"
    -- ... a | e,ai ...
    -- ...   ai     ...
    -- ... a | o,au ...
    -- ...   au     ...
    | qualityOf left == "a" && inSutra right "e" "c" =
        let aTrace = (trace ++ ".L-a") in
        MergedBoundary
            (leftRem, [diphthongize right], rightRem)
            (aTrace ++ "-diph [6.1.88]")

    -- 6.1.78 "ec-aH ay-av-a:y-a:v-aH" -> "eco 'yava:ya:v-aH"
    | inSutra left "e" "G" =
        -- ... e,o | V ...
        -- ...  a  | V ...
        RetainedBoundary
            (leftRem, Just (confirmPhoneme "a")) (Just right, rightRem)
            (trace ++ ".L-smooth [6.1.78]")
    | soundOf left == "au" =
        -- ... au | V ...
        -- ...  a:vV  ...
        MergedBoundary
            (leftRem, [confirmPhoneme "a:", approximantize left, right], rightRem)
            (trace ++ ".L-au-approx [6.1.78]")
    | soundOf left == "ai" =
        -- ... ai | V ...
        -- ... a: | V ...
        RetainedBoundary
            (leftRem, Just (confirmPhoneme "a:")) (Just right, rightRem)
            (trace ++ ".L-smooth [6.1.78]")

    | otherwise = UnchangedPhonemes (left, right)
