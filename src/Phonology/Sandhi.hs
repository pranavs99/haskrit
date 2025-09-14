module Phonology.Sandhi where

import Phonology.Phoneme.Types
import Phonology.Phoneme.Inventory
import Phonology.Phoneme
import Phonology.Utilities as Utils


data BoundarySandhiResult
    = SandhifiedPhonemes [Phoneme]
    | UnchangedPhonemes (Phoneme, Phoneme)
    deriving (Show)

data SandhiResult
    = Sandhita String
    | UnchangedWords (String, String)
    deriving (Show, Eq)


{-
    savarNa
    def) two vowels with the same quality
    --> only the qualities "a", "i", and "u" can really exhibit this,
        so qualityOf can only match with those vowels
-}
matchesSavarNa :: Phoneme -> Phoneme -> Bool
matchesSavarNa leftVowel rightVowel =
    qualityOf leftVowel == qualityOf rightVowel

applySavarNa :: Phoneme -> Phoneme -> BoundarySandhiResult
applySavarNa leftVowel rightVowel = case qualityOf leftVowel of
    "a" -> SandhifiedPhonemes [confirmPhoneme "a:"]
    "i" -> SandhifiedPhonemes [confirmPhoneme "i:"]
    "u" -> SandhifiedPhonemes [confirmPhoneme "u:"]
    _   -> UnchangedPhonemes (leftVowel, rightVowel)

applyVocalicSandhi :: Phoneme -> Phoneme -> BoundarySandhiResult
applyVocalicSandhi leftVowel rightVowel
    | matchesSavarNa leftVowel rightVowel =
        applySavarNa leftVowel rightVowel
    | otherwise =
        UnchangedPhonemes (leftVowel, rightVowel)

applySandhi :: Phoneme -> Phoneme -> BoundarySandhiResult
applySandhi leftPhoneme rightPhoneme
    | isVowel leftPhoneme && isVowel rightPhoneme =
        applyVocalicSandhi leftPhoneme rightPhoneme
    | otherwise =
        UnchangedPhonemes (leftPhoneme, rightPhoneme)

applySandhiToPair :: String -> String -> SandhiResult
applySandhiToPair firstWord secondWord =
    let
        firstWordPhonemes = phonemesOf firstWord
        (firstWordRemainder, leftContext) =
            Utils.splitLast firstWordPhonemes
        secondWordPhonemes = phonemesOf secondWord
        (rightContext, secondWordRemainder) =
            Utils.splitFirst secondWordPhonemes
    in
        case applySandhi leftContext rightContext of
            SandhifiedPhonemes sandhifiedPhonemes ->
                Sandhita
                    (concatMap soundOf firstWordRemainder
                    ++ concatMap soundOf sandhifiedPhonemes
                    ++ concatMap soundOf secondWordRemainder)
            UnchangedPhonemes _ ->
                UnchangedWords (firstWord, secondWord)

applySandhiToWords :: [String] -> [String]
applySandhiToWords sentenceWords =
    reverse (sandhify sentenceWords Nothing []) where
    sandhify :: [String] -> Maybe String -> [String] -> [String]
    sandhify [] Nothing sandhitaAcc =
        sandhitaAcc
    sandhify [] (Just currSandhita) sandhitaAcc =
        currSandhita : sandhitaAcc
    sandhify (firstWord : rest) Nothing sandhitaAcc =
        sandhify rest (Just firstWord) sandhitaAcc
    sandhify (firstWord : rest) (Just currSandhita) sandhitaAcc =
        case applySandhiToPair currSandhita firstWord of
            Sandhita newSandhita ->
                sandhify rest (Just newSandhita) sandhitaAcc
            UnchangedWords (prevSandhita, _) ->
                sandhify rest (Just firstWord) (prevSandhita : sandhitaAcc)

applySandhiToSentence :: String -> String
applySandhiToSentence sentence =
    Prelude.unwords (applySandhiToWords (Prelude.words sentence))
