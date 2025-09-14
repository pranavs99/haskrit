module Phonology.Phoneme where

import Phonology.Phoneme.Types
import Phonology.Phoneme.Inventory


isVowel :: Phoneme -> Bool
isVowel (Vowel _ _) = True
isVowel _           = False

isConsonant :: Phoneme -> Bool
isConsonant (Consonant _ _) = True
isConsonant _               = False

soundOf :: Phoneme -> String
soundOf (Vowel sound _)     = sound
soundOf (Consonant sound _) = sound

qualityOf :: Phoneme -> String
qualityOf (Vowel sound _) = case head sound of
    'a' -> "a"
    'i' -> "i"
    'u' -> "u"
    _   -> sound
qualityOf (Consonant sound _) = sound

phonemesOf :: String -> [Phoneme]
phonemesOf word = parse word [] where
    parse :: String -> [Phoneme] -> [Phoneme]
    parse "" phonemeAcc = reverse phonemeAcc
    parse [c] phonemeAcc = case matchPhoneme [c] of
        Just monograph  -> parse [] (monograph : phonemeAcc)
        Nothing         -> error ("unrecognized phoneme: " ++ [c])
    parse (c1 : (c2 : rest)) phonemeAcc = case matchPhoneme [c1, c2] of
        Just digraph    -> parse rest (digraph : phonemeAcc)
        Nothing         -> case matchPhoneme [c1] of
            Just monograph  -> parse (c2 : rest) (monograph : phonemeAcc)
            Nothing         -> error ("unrecognized phoneme: " ++ [c1])

soundsOf :: String -> [String]
soundsOf word = map soundOf (phonemesOf word)
