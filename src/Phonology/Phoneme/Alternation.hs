module Phonology.Phoneme.Alternation where

import Phonology.Phoneme
import Phonology.Phoneme.Types
import Phonology.Phoneme.Feature


{- atomic feature alternation

    input:  {new}   a Feature to alternate
            {olds}  the original list of Features
    output: a list of Features with only {new} alternated

    returns {olds} if Feature to alternate not found in {olds}
-}
alternateFeature :: Feature -> [Feature] -> [Feature]
alternateFeature _ [] = []
alternateFeature new (currOld : restOlds)
    | featuresMatch new currOld =
        new : restOlds
    | otherwise =
        currOld : alternateFeature new restOlds

{- feature alternation

    input:  {news}  a list of Features to alternate
            {olds}  the original list of Features
    output: a list of Features with {news} alternated
-}
alternateFeatures :: [Feature] -> [Feature] -> [Feature]
alternateFeatures [] olds = olds
alternateFeatures (currNew : restNews) olds =
    alternateFeatures restNews (alternateFeature currNew olds)

{- phonological alternation

    input:  {targets}           a list of Features to alternate
            {constraintIds}     a list of feature IDs for enumerated constraints
                [] <==> all {phoneme} Features should be constraints
            {phoneme}           the original Phoneme being alternated
    output: the alternated Phoneme

    errors: (from confirmPhonemeOn)
            if no Phoneme matches {targets} and constraints
-}
alternate :: [Feature] -> [String] -> Phoneme -> Phoneme
alternate targets constraintIds phoneme =
    alt targets constraintIds (featuresOf phoneme) where
    alt :: [Feature] -> [String] -> [Feature] -> Phoneme
    alt news [] features =
        confirmPhonemeOn (alternateFeatures news features)
    alt news ids _ =
        confirmPhonemeOn (news ++ findFeatures ids phoneme)

--------------------------------------------------------------------------------
{- common alternations -}
--------------------------------------------------------------------------------

elongate :: Phoneme -> Phoneme
elongate        = alternate [Length Long]           []

compositize :: Phoneme -> Phoneme
compositize     = alternate [VowelClass Composite]  ["place"]

diphthongize :: Phoneme -> Phoneme
diphthongize    = alternate [VowelClass Diphthong]  ["place"]

palatalize :: Phoneme -> Phoneme
palatalize      = alternate [Place Palatal]         ["manner"]

approximantize :: Phoneme -> Phoneme
approximantize  = alternate [Manner Approximant]    ["place"]

spirantize :: Phoneme -> Phoneme
spirantize      = alternate [Manner Fricative]      ["place"]
