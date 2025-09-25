module Phonology.Phoneme.Inventory where

import Phonology.Phoneme.Types
import Phonology.Phoneme.Feature


vowels :: [Phoneme]
vowels = [
    Vowel "a"
        [Length Short, Place Velar, VowelClass Monophthong, Manner Vocalic],
    Vowel "a:"
        [Length Long, Place Velar, VowelClass Monophthong, Manner Vocalic],
    Vowel "i"
        [Length Short, Place Palatal, VowelClass Monophthong, Manner Vocalic],
    Vowel "i:"
        [Length Long, Place Palatal, VowelClass Monophthong, Manner Vocalic],
    Vowel "u"
        [Length Short, Place Labial, VowelClass Monophthong, Manner Vocalic],
    Vowel "u:"
        [Length Long, Place Labial, VowelClass Monophthong, Manner Vocalic],
    Vowel "R"
        [Length Short, Place Retroflex, VowelClass Monophthong, Manner Vocalic],
    Vowel "R:"
        [Length Long, Place Retroflex, VowelClass Monophthong, Manner Vocalic],
    Vowel "L"
        [Length Short, Place Dental, VowelClass Monophthong, Manner Vocalic],
    Vowel "L:"
        [Length Long, Place Dental, VowelClass Monophthong, Manner Vocalic],
    Vowel "e"
        [Place Palatal, VowelClass Composite, Manner Vocalic],
    Vowel "ai"
        [Place Palatal, VowelClass Diphthong, Manner Vocalic],
    Vowel "o"
        [Place Labial, VowelClass Composite, Manner Vocalic],
    Vowel "au"
        [Place Labial, VowelClass Diphthong, Manner Vocalic],
    Vowel "M"
        [Manner Vocalic],
    Vowel "H"
        [Manner Vocalic]]

consonants :: [Phoneme]
consonants = [
    -- velar consonants
    Consonant "k"
        [Voicing Unvoiced, Aspiration Unaspirated, Place Velar, Manner Plosive],
    Consonant "kh"
        [Voicing Unvoiced, Aspiration Aspirated, Place Velar, Manner Plosive],
    Consonant "g"
        [Voicing Voiced, Aspiration Unaspirated, Place Velar, Manner Plosive],
    Consonant "gh"
        [Voicing Voiced, Aspiration Aspirated, Place Velar, Manner Plosive],
    Consonant "G"
        [Place Velar, Manner Nasal],
    -- palatal consonants
    Consonant "c"
        [Voicing Unvoiced, Aspiration Unaspirated, Place Palatal, Manner Plosive],
    Consonant "ch"
        [Voicing Unvoiced, Aspiration Aspirated, Place Palatal, Manner Plosive],
    Consonant "j"
        [Voicing Voiced, Aspiration Unaspirated, Place Palatal, Manner Plosive],
    Consonant "jh"
        [Voicing Voiced, Aspiration Aspirated, Place Palatal, Manner Plosive],
    Consonant "ny"
        [Place Palatal, Manner Nasal],
    -- retroflex consonants
    Consonant "T"
        [Voicing Unvoiced, Aspiration Unaspirated, Place Retroflex, Manner Plosive],
    Consonant "Th"
        [Voicing Unvoiced, Aspiration Aspirated, Place Retroflex, Manner Plosive],
    Consonant "D"
        [Voicing Voiced, Aspiration Unaspirated, Place Retroflex, Manner Plosive],
    Consonant "Dh"
        [Voicing Voiced, Aspiration Aspirated, Place Retroflex, Manner Plosive],
    Consonant "N"
        [Place Retroflex, Manner Nasal],
    -- dental consonants
    Consonant "t"
        [Voicing Unvoiced, Aspiration Unaspirated, Place Dental, Manner Plosive],
    Consonant "th"
        [Voicing Unvoiced, Aspiration Aspirated, Place Dental, Manner Plosive],
    Consonant "d"
        [Voicing Voiced, Aspiration Unaspirated, Place Dental, Manner Plosive],
    Consonant "dh"
        [Voicing Voiced, Aspiration Aspirated, Place Dental, Manner Plosive],
    Consonant "n"
        [Place Dental, Manner Nasal],
    -- labial consonants
    Consonant "p"
        [Voicing Unvoiced, Aspiration Unaspirated, Place Bilabial, Manner Plosive],
    Consonant "ph"
        [Voicing Unvoiced, Aspiration Aspirated, Place Bilabial, Manner Plosive],
    Consonant "b"
        [Voicing Voiced, Aspiration Unaspirated, Place Bilabial, Manner Plosive],
    Consonant "bh"
        [Voicing Voiced, Aspiration Aspirated, Place Bilabial, Manner Plosive],
    Consonant "m"
        [Place Bilabial, Manner Nasal],
    -- approximants
    Consonant "y"
        [Place Palatal, Manner Approximant],
    Consonant "r"
        [Place Retroflex, Manner Approximant],
    Consonant "l"
        [Place Dental, Manner Approximant],
    Consonant "v"
        [Place Labial, Manner Approximant],
    -- fricatives
    Consonant "h"
        [Voicing Voiced, Place Velar, Manner Fricative],
    Consonant "sh"
        [Voicing Unvoiced, Place Palatal, Manner Fricative],
    Consonant "S"
        [Voicing Unvoiced, Place Retroflex, Manner Fricative],
    Consonant "s"
        [Voicing Unvoiced, Place Dental, Manner Fricative]]

phonemes :: [Phoneme]
phonemes = vowels ++ consonants
