module Phonology.Sandhi where

import Phonology.Phoneme.Types
import Phonology.Phoneme
import Phonology.Sandhi.Types
import Phonology.Sandhi.Vocalic
import Phonology.Utilities as Utils


{-
    identify sandhi contexts and apply corresponding sandhi rules

    input: left and right context as Phonemes
    output: either a successful boundary sandhi,
            or a tuple containing the unchanged contexts
-}
applySandhiToPhonemes :: Phoneme -> Phoneme -> BoundarySandhiResult
applySandhiToPhonemes leftContext rightContext
    | isVowel leftContext && isVowel rightContext =
        Phonology.Sandhi.Vocalic.applyVocalicSandhi leftContext rightContext
    | otherwise =
        UnchangedPhonemes (leftContext, rightContext)

{-
    extract sandhi contexts and stitch successful sandhi application results
    into a Sandhita constructor

    input: two contiguous words (can also be previous sandhitas) as Strings
    output: either a successful sandhita, or a tuple containing the unchanged
            input words
-}
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
        case applySandhiToPhonemes leftContext rightContext of
            SandhifiedPhonemes sandhifiedPhonemes ->
                Sandhita
                    (concatMap soundOf firstWordRemainder
                    ++ concatMap soundOf sandhifiedPhonemes
                    ++ concatMap soundOf secondWordRemainder)
            UnchangedPhonemes _ ->
                UnchangedWords (firstWord, secondWord)

{-
    apply sandhi over a list of words

    input: list of words as Strings
    output: successful sandhitas and unchanged words as Strings
    --> keeps accumulating sandhitas if consecutive sandhi application succeeds
-}
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

{-
    entrypoint for sandhi application

    input: sentence as String
    output: sandhified sentence as String
-}
applySandhi :: String -> String
applySandhi sentence =
    Prelude.unwords (applySandhiToWords (Prelude.words sentence))

{-
    IO testing function for sandhi application

    input: sentence as String
    output: sandhified interaction as IO ()
-}
runSandhi :: IO ()
runSandhi = do
    putStrLn ""
    putStrLn "enter sentence to apply sandhi to:"
    sentence <- getLine
    putStrLn ""
    putStrLn (applySandhi sentence)
    putStrLn ""
