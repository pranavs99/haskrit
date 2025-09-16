module Phonology.Phoneme.Sutra where

import Phonology.Utilities as Utils


data Sutra
    = Sutra [String] String
    deriving (Show)

shivaSutras :: [Sutra]
shivaSutras = [
    Sutra ["a", "i", "u"] "N1",
    Sutra ["R", "L"] "k",
    Sutra ["e", "o"] "G",
    Sutra ["ai", "au"] "c",
    Sutra ["h", "y", "v", "r"] "T",
    Sutra ["l"] "N2",
    Sutra ["ny", "m", "G", "N", "n"] "m",
    Sutra ["jh", "bh"] "ny",
    Sutra ["gh", "Dh", "dh"] "S",
    Sutra ["j", "b", "g", "D", "d"] "sh",
    Sutra ["kh", "ph", "ch", "Th", "th", "c", "T", "t"] "v",
    Sutra ["k", "p"] "y",
    Sutra ["sh", "S", "s"] "r",
    Sutra ["h"] "l"]

isSutraMarker :: String -> Bool
isSutraMarker marker = match marker shivaSutras where
    match :: String -> [Sutra] -> Bool
    match _ [] = False
    match givenMarker (sutra : rest) = case sutra of
        Sutra _ currMarker ->
            (currMarker == givenMarker) || match givenMarker rest

collectFromSutra :: String -> Sutra -> Maybe [String]
collectFromSutra startSound (Sutra sutraSounds _) =
    collect startSound sutraSounds where
    collect :: String -> [String] -> Maybe [String]
    collect _ [] = Nothing
    collect start (curr : rest)
        | start == curr = Just (curr : rest)
        | otherwise     = collect start rest

matchSutra :: String -> String -> [String]
matchSutra startSound endMarker =
    match startSound endMarker shivaSutras Nothing where
    match ::
        String -> String -> [Sutra] -> Maybe [String] -> [String]
    match _ _ [] _ =
        error
            ("matchSutra: no Sutra matches: "
            ++ startSound ++ " -> " ++ endMarker)
    match start marker (currSutra : restSutras) soundAcc = case currSutra of
        Sutra sutraSounds currMarker ->
            if currMarker == marker then case soundAcc of
                Just collectedSounds ->
                    collectedSounds ++ sutraSounds
                Nothing ->
                    error
                        ("matchSutra: marker perhaps too early: " ++ marker)
            else case collectFromSutra start currSutra of
                Just currCollected ->
                    match start marker restSutras (Just currCollected)
                Nothing -> case soundAcc of
                    Just allCollected ->
                        let moreAdded = Just (allCollected ++ sutraSounds) in
                        match start marker restSutras moreAdded
                    Nothing ->
                        match start marker restSutras Nothing

runMatchSutra :: String -> String -> IO ()
runMatchSutra startSound endMarker
    | isSutraMarker endMarker =
        printStrings (matchSutra startSound endMarker)
    | otherwise =
        putStrLn ("not valid sutra marker: " ++ endMarker)
