module Phonology.Sandhi.Consonantal where

import Phonology.Sandhi.Types
import Phonology.Phoneme
import Phonology.Phoneme.Alternation
import Phonology.Phoneme.Feature
import Phonology.Phoneme.Types
import Phonology.Utilities as Utils


{-
    apply consonantal word boundary sandhi

    inputs: - trace (String)
            - (leftRem, left) (right, rightRem)
    output:
-}
applyBoundaryHal
    :: String
    -> ([Phoneme], Phoneme) -> (Phoneme, [Phoneme])
    -> BoundarySandhiResult
applyBoundaryHal trace (leftRem, left) (right, rightRem)

    -- 8.3.23 "ma-H" "anusva:r-aH" -> "mo 'nusva:r-aH"
    | soundOf left == "m" && inSutra right "jh" "l" =
        anusvaara trace (leftRem, left) (right, rightRem)

    -- 8.3.15 "khar-avasa:nayoH visarjani:yam" -> "kharavasa:nayorvisarjani:yam"
    | soundOf left `elem` ["r", "s"] =
        visarga trace (leftRem, left) (right, rightRem)

    | otherwise = UnchangedPhonemes (left, right)

{-  8.3.23

    m-aH anusva:r-aH -> mo 'nusva:raH
    m    -> anusva:ra [word-finally when C follows]

    ... /m/ | C ...
    ...  M  | C ...

    ex) nagaram gaccha:mi
              | |
        nagaraM gaccha:mi
        "I go to the city"
-}
anusvaara
    :: String
    -> ([Phoneme], Phoneme) -> (Phoneme, [Phoneme])
    -> BoundarySandhiResult
anusvaara trace (leftRem, _) (right, rightRem) =
    RetainedBoundary
        (leftRem, Just (confirmPhoneme "M")) (Just right, rightRem)
        (trace ++ "/anusva:ra")

{-  visarga sandhi
    .../r/
    .../s/ | ...
-}
visarga
    :: String
    -> ([Phoneme], Phoneme) -> (Phoneme, [Phoneme])
    -> BoundarySandhiResult
visarga trace (leftRem, left) (right, rightRem)

    {-  8.3.14

        ra-H r-i
        r    -> ∅ b/f r
        word-final "r" becomes lopa before "r"

        .../r/ | /r/...
        ... ∅  | /r/...

    ex) dva:r rohita:
            | |
        dva:  rohita:
        "the door is red"

    --> function brought to front because r | r environment
    --> can be matched easily
    -}
    | soundOf left == "r" && soundOf right == "r" =
        RetainedBoundary
            (leftRem, Nothing) (Just right, rightRem)
            (trace ++ "/visarga.L-lopa")

    {-  8.3.15

        khar                ava:sanayoH visarjani:yam
        b/f (kh -> r) &     b/f #       r -> H
        word-final "r" becomes visarga before (kh -> r) or #

        .../r/ | C [-voice] ...
        ... H  | C [-voice] ...

    ex) ra:mas kha:dati
             | |
        ra:maH kha:dati
        "Rama eats"
    -}
    | soundOf left == "s" && inSutra right "kh" "r" =
        -- aH postVisarga entrypoint
        postVisarga (trace ++ "/visarga") (leftRem, left) (right, rightRem)

    | otherwise = UnchangedPhonemes (left, right)

{-  2nd visarga sandhi layer

    word-final /r,s/ become H, which undergoes this further sandhi:

    ex) ra:m(aH) -> o [+]
-}
postVisarga
    :: String
    -> ([Phoneme], Phoneme) -> (Phoneme, [Phoneme])
    -> BoundarySandhiResult
postVisarga trace (leftRem, left) (right, rightRem)

    | isVoiced (featuresOf right) =

        -- ... aH | C[+v] ...
        -- ...  o | C[+v] ...
        let
            (restBefores, lastBefore)   = Utils.splitLast leftRem
            (leftExcept2, _)            = Utils.splitLast restBefores
        in
            if isConsonant right && soundOf lastBefore == "a" then
                RetainedBoundary
                    (leftExcept2, Just (confirmPhoneme "o")) (Just right, rightRem)
                    (trace ++ ".L-aH")
            -- ... (~a)H | [+v] ...
            -- ...       | [+v] ...
            else
                RetainedBoundary
                    (leftRem, Nothing) (Just right, rightRem)
                    (trace ++ ".L-lopa")

    --        H     | /t,th,c,ch/ ...
    -- ... s[place] | /t,th,c,ch/ ...
    -- [X] Sutra underpowered for "t", "th", "c", "ch"
    | soundOf right `elem` ["t", "th", "c", "ch"] =
        let fricative = spirantize right in
        RetainedBoundary
            (leftRem, Just fricative) (Just right, rightRem)
            (trace ++ ".R-spir")

    | otherwise = UnchangedPhonemes (left, right)
