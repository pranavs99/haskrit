module Phonology.Phoneme.Inventory where

import Phonology.Phoneme.Types
import Phonology.Feature


matchPhonemeFrom :: String -> [Phoneme] -> Maybe Phoneme
matchPhonemeFrom _ [] = Nothing
matchPhonemeFrom s (ph : rest) = case ph of
    Vowel sound fs
        | sound == s -> Just (Vowel sound fs)
    Consonant sound fs
        | sound == s -> Just (Consonant s fs)
    _ -> matchPhonemeFrom s rest

matchPhoneme :: String -> Maybe Phoneme
matchPhoneme s = matchPhonemeFrom s phonemes

confirmPhoneme :: String -> Phoneme
confirmPhoneme s = case matchPhoneme s of
    Just ph -> ph
    _       -> error "invariant violated: not a phoneme"

vowels :: [Phoneme]
vowels = map addVocalicFeature [
    Vowel "a" [],
    Vowel "a:" [Length Long],
    Vowel "i" [],
    Vowel "i:" [Length Long],
    Vowel "u" [],
    Vowel "u:" [Length Long],
    Vowel "e" [],
    Vowel "ai" [Length Long],
    Vowel "o" [],
    Vowel "au" [Length Long],
    Vowel "aM" [],
    Vowel "aH" []]

addVocalicFeature :: Phoneme -> Phoneme
addVocalicFeature ph = case ph of
    Vowel sound features    -> Vowel sound (Manner Vocalic : features)
    other                   -> other

consonants :: [Phoneme]
consonants = [
    -- velar consonants
    Consonant "k" [Voicing Unvoiced, Aspiration Unaspirated, Place Velar, Manner Plosive],
    Consonant "kh"  [Voicing Unvoiced, Aspiration Aspirated, Place Velar, Manner Plosive],
    Consonant "g"   [Voicing Voiced, Aspiration Unaspirated, Place Velar, Manner Plosive],
    Consonant "gh"  [Voicing Voiced, Aspiration Aspirated, Place Velar, Manner Plosive],
    Consonant "G"   [Place Velar, Manner Nasal],
    -- palatal consonants
    Consonant "c"   [Voicing Unvoiced, Aspiration Unaspirated, Place Palatal, Manner Plosive],
    Consonant "ch"  [Voicing Unvoiced, Aspiration Aspirated, Place Palatal, Manner Plosive],
    Consonant "j"   [Voicing Voiced, Aspiration Unaspirated, Place Palatal, Manner Plosive],
    Consonant "jh"  [Voicing Voiced, Aspiration Aspirated, Place Palatal, Manner Plosive],
    Consonant "ny"  [Place Palatal, Manner Nasal],
    -- retroflex consonants
    Consonant "T"   [Voicing Unvoiced, Aspiration Unaspirated, Place Retroflex, Manner Plosive],
    Consonant "Th"  [Voicing Unvoiced, Aspiration Aspirated, Place Retroflex, Manner Plosive],
    Consonant "D"   [Voicing Voiced, Aspiration Unaspirated, Place Retroflex, Manner Plosive],
    Consonant "Dh"  [Voicing Voiced, Aspiration Aspirated, Place Retroflex, Manner Plosive],
    Consonant "N"   [Place Retroflex, Manner Nasal],
    -- dental consonants
    Consonant "t"   [Voicing Unvoiced, Aspiration Unaspirated, Place Dental, Manner Plosive],
    Consonant "th"  [Voicing Unvoiced, Aspiration Aspirated, Place Dental, Manner Plosive],
    Consonant "d"   [Voicing Voiced, Aspiration Unaspirated, Place Dental, Manner Plosive],
    Consonant "dh"  [Voicing Voiced, Aspiration Aspirated, Place Dental, Manner Plosive],
    Consonant "n"   [Place Dental, Manner Nasal],
    -- labial consonants
    Consonant "p"   [Voicing Unvoiced, Aspiration Unaspirated, Place Labial, Manner Plosive],
    Consonant "ph"  [Voicing Unvoiced, Aspiration Aspirated, Place Labial, Manner Plosive],
    Consonant "b"   [Voicing Voiced, Aspiration Unaspirated, Place Labial, Manner Plosive],
    Consonant "bh"  [Voicing Voiced, Aspiration Aspirated, Place Labial, Manner Plosive],
    Consonant "m"   [Place Labial, Manner Nasal],
    -- approximants
    Consonant "y"   [Place Palatal, Manner Approximant],
    Consonant "r"   [Place Retroflex, Manner Approximant],
    Consonant "l"   [Place Dental, Manner Approximant],
    Consonant "v"   [Place Labial, Manner Approximant],
    -- fricatives
    Consonant "h"   [Voicing Voiced, Place Velar, Manner Fricative],
    Consonant "sh"  [Voicing Unvoiced, Place Palatal, Manner Fricative],
    Consonant "S"   [Voicing Unvoiced, Place Retroflex, Manner Fricative],
    Consonant "s"   [Voicing Unvoiced, Place Dental, Manner Fricative]]

phonemes :: [Phoneme]
phonemes = vowels ++ consonants

{-
    phoneme matching

    i wanted to move this to Phoneme.hs, but matchPhoneme must perform a valid
    match from the enumerated inventory
-}
