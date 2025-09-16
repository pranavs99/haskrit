module Phonology.Phoneme.Alternation where

import Phonology.Phoneme
import Phonology.Phoneme.Types
import Phonology.Phoneme.Feature


changeManner :: MannerType -> [Feature] -> Maybe [Feature]
changeManner newPlace features = change newPlace features [] where
    change :: MannerType -> [Feature] -> [Feature] -> Maybe [Feature]
    change _ [] _ = Nothing
    change new (currFeature : restFeatures) featureAcc =
        case currFeature of
            Manner _    -> Just (Manner new : (featureAcc ++ restFeatures))
            _           -> change new restFeatures (currFeature : featureAcc)

{-
    vowel elongation
    input: a Phoneme
    output: maybe a lengthened version of the Phoneme
    --> only a subset of Vowels can be lengthened
    --> the parallel of elongation for Consonants is gemination
-}
elongate :: Phoneme -> Maybe Phoneme
elongate (Consonant _ _) = Nothing
elongate (Vowel sound features)
    | Length Long `elem` features =
        Just (Vowel sound features)
    | otherwise =
        matchPhoneme (Length Long : features)
