module Phonology.Phoneme where

import Phonology.Phoneme.Types
import Phonology.Phoneme.Inventory
import Phonology.Phoneme.Feature
import Phonology.Utilities as Utils


{-
    simple accessor functions
-}

isVowel :: Phoneme -> Bool
isVowel (Vowel _ _) = True
isVowel _           = False

isConsonant :: Phoneme -> Bool
isConsonant (Consonant _ _) = True
isConsonant _               = False

soundOf :: Phoneme -> String
soundOf (Vowel sound _)     = sound
soundOf (Consonant sound _) = sound

soundsMatch :: Phoneme -> Phoneme -> Bool
soundsMatch = Utils.predicateMatches soundOf

qualityOf :: Phoneme -> String
qualityOf (Vowel sound _) = case head sound of
    'a' -> "a"
    'i' -> "i"
    'u' -> "u"
    _   -> sound
qualityOf (Consonant sound _) = sound

qualitiesMatch :: Phoneme -> Phoneme -> Bool
qualitiesMatch = Utils.predicateMatches qualityOf

featuresOf :: Phoneme -> [Feature]
featuresOf (Vowel _ features)       = features
featuresOf (Consonant _ features)   = features

{-
    input: sound as String
    output: possibly the Phoneme with the given sound
-}
lookupPhoneme :: String -> Maybe Phoneme
lookupPhoneme sound = match sound Phonology.Phoneme.Inventory.phonemes where
    match :: String -> [Phoneme] -> Maybe Phoneme
    match _ [] = Nothing
    match s (currPhoneme : restPhonemes)
        | soundOf currPhoneme == s =
            Just currPhoneme
        | otherwise =
            match s restPhonemes

{-
    input: sound as String
    output: the Phoneme with the given sound
    errors: if no Phoneme found

    --> for use when a sound is more guaranteed to produce a Phoneme
-}
confirmPhoneme :: String -> Phoneme
confirmPhoneme sound = case lookupPhoneme sound of
    Just ph -> ph
    _       -> error "invariant violated: not a phoneme"

{-
    input: given features as [Feature]
    output: possibly a Phoneme that contains all given features
-}
matchPhoneme :: [Feature] -> Maybe Phoneme
matchPhoneme givenFeatures = match givenFeatures phonemes where
    match :: [Feature] -> [Phoneme] -> Maybe Phoneme
    match _ [] = Nothing
    match given (currPhoneme : restPhonemes)
        | Utils.isSubsetOf given (featuresOf currPhoneme) =
            Just currPhoneme
        | otherwise =
            match given restPhonemes

{-
    input: word as String
    output: a list of contiguous Phonemes matching the sounds in a word
    errors: if any characters are not recognized as Phonemes
-}
phonemesOf :: String -> [Phoneme]
phonemesOf word = parse word [] where
    parse :: String -> [Phoneme] -> [Phoneme]
    parse "" phonemeAcc = reverse phonemeAcc
    parse [c] phonemeAcc = case lookupPhoneme [c] of
        Just monograph  -> parse [] (monograph : phonemeAcc)
        Nothing         -> error ("unrecognized phoneme: " ++ [c])
    parse (c1 : (c2 : rest)) phonemeAcc = case lookupPhoneme [c1, c2] of
        Just digraph ->
            parse rest (digraph : phonemeAcc)
        Nothing -> case lookupPhoneme [c1] of
            Just monograph  -> parse (c2 : rest) (monograph : phonemeAcc)
            Nothing         -> error ("unrecognized phoneme: " ++ [c1])

{-
    input: word as String
    output: a list of sounds from valid Phonemes as [String]
    errors: if phonemesOf finds an invalid Phoneme
-}
soundsOf :: String -> [String]
soundsOf word = map soundOf (phonemesOf word)
