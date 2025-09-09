module Phonology.Phoneme where

import Phonology.Phoneme.Types
import Phonology.Phoneme.Inventory


isVowel :: Phoneme -> Bool
isVowel (Vowel _ _) = True
isVowel _           = False

isConsonant :: Phoneme -> Bool
isConsonant (Consonant _ _) = True
isConsonant _               = False

phonemeToString :: Phoneme -> String
phonemeToString (Vowel name _)      = name
phonemeToString (Consonant name _)  = name
phonemeToString (Unrecognized name) = name

phonemesOf :: String -> Either String [Phoneme]
phonemesOf word = parse word [] where
    parse :: String -> [Phoneme] -> Either String [Phoneme]
    parse "" phonemeAcc = Right (reverse phonemeAcc)
    parse [c] phonemeAcc = case matchPhoneme [c] of
        Just monograph  -> parse [] (monograph : phonemeAcc)
        Nothing         -> Left ("unrecognized phoneme: " ++ [c])
    parse (c1 : (c2 : rest)) phonemeAcc = case matchPhoneme [c1, c2] of
        Just digraph    -> parse rest (digraph : phonemeAcc)
        Nothing         -> case matchPhoneme [c1] of
            Just monograph  -> parse (c2 : rest) (monograph : phonemeAcc)
            Nothing         -> Left ("unrecognized phoneme: " ++ [c1])

phonemeNames :: String -> Either String [String]
phonemeNames word = case phonemesOf word of
    Left err    -> Left err
    Right phs   -> Right (map phonemeToString phs)
