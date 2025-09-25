module Phonology.Phoneme.Sutra where

import Phonology.Utilities as Utils

import Data.List (intercalate)


data Sutra = Sutra [String] String

soundsOfSutra :: Sutra -> [String]
soundsOfSutra (Sutra sounds _) = sounds

markerOfSutra :: Sutra -> String
markerOfSutra (Sutra _ marker) = marker

shivaSutras :: [Sutra]
shivaSutras = [
    Sutra ["a", "a:", "i", "i:", "u", "u:"] "N1",
    Sutra ["R", "R:", "L", "L:"] "k",
    Sutra ["e", "o"] "G",
    Sutra ["ai", "au"] "c",
    Sutra ["h1", "y", "v", "r"] "T",
    Sutra ["l"] "N2",
    Sutra ["ny", "m", "G", "N", "n"] "m",
    Sutra ["jh", "bh"] "ny",
    Sutra ["gh", "Dh", "dh"] "S",
    Sutra ["j", "b", "g", "D", "d"] "sh",
    Sutra ["kh", "ph", "ch", "Th", "th", "c", "T", "t"] "v",
    Sutra ["k", "p"] "y",
    Sutra ["sh", "S", "s"] "r",
    Sutra ["h2"] "l"]

collectFromSutra :: String -> Sutra -> Maybe [String]
collectFromSutra start (Sutra sounds _) =
    case Utils.splitAt start sounds of
        (_, [])             -> Nothing
        (_, startAndRest)   -> Just startAndRest

matchSutra :: String -> String -> [String]
matchSutra start marker = match start marker shivaSutras Nothing where
    match
        :: String           -- start sound
        -> String           -- end marker
        -> [Sutra]          -- Shiva Sutras as ground truth
        -> Maybe [String]   -- Just (accumulated sounds) / Nothing (no collection yet)
        -> [String]         -- result: sounds matching Sutra
        -- errors: if no Sutra matches
    match givenStart givenMarker [] _ =
        -- nothing was collected, Sutra was invalid
        Utils.errorMessages
            [ "(matchSutra) NoSutraMatched" ++ "\n"
            , "input: " ++ givenStart ++ " -> " ++ givenMarker ++ "\n"]
    -- looking to START
    match givenStart givenMarker (currSutra : restSutras) Nothing =
        case collectFromSutra givenStart currSutra of
            Just startedSounds ->
                if markerOfSutra currSutra == givenMarker then
                    startedSounds
                else
                    match givenStart givenMarker restSutras (Just startedSounds)
            Nothing ->
                match givenStart givenMarker restSutras Nothing
    match givenStart givenMarker (currSutra : restSutras) (Just soundAcc) =
        case collectFromSutra givenStart currSutra of
            Nothing ->
                let moreSounds = soundAcc ++ soundsOfSutra currSutra in
                if markerOfSutra currSutra == givenMarker then
                    moreSounds
                else
                    match givenStart givenMarker restSutras (Just moreSounds)
            _ -> Utils.errorMessages
                [ "(matchSutra) accumulator non-empty when collection started"
                , "input: " ++ givenStart ++ " -> " ++ givenMarker ++ "\n"
                , "accumulator: " ++ show soundAcc ]

encodeMatchSutraResult :: String -> String -> [String] -> String
encodeMatchSutraResult start marker matched =
    "(matchSutra)" ++ "\n"
    ++ "input:          " ++ start ++ " -> " ++ marker ++ "\n"
    ++ "matched Sutra:  " ++ intercalate ", " matched

runMatchSutra :: IO String
runMatchSutra = do
    putStrLn ""
    putStrLn "starting sound:"
    start <- getLine
    putStrLn ""
    putStrLn "ending marker:"
    marker <- getLine
    let matched = matchSutra start marker
    putStrLn ""
    putStrLn "matched Sutra:"
    Utils.printStrings matched
    return (encodeMatchSutraResult start marker matched)

-- ञ म ङ ण न म्
-- ña ma ṅa ṇa na m

-- झ भ ञ्
-- jha bha ñ

-- घ ढ ध ष्
-- gha ḍha dha ṣ

-- ज ब ग ड द श्
-- ja ba ga ḍa da ś

-- ख फ छ ठ थ च ट त व्
-- kha pha cha ṭha tha ca ṭa ta v

-- क प य्
-- ka pa y

-- श ष स र्
-- śa ṣa sa r

-- ह ल्
-- ha l
