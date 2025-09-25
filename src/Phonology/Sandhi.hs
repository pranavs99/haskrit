module Phonology.Sandhi where


import Phonology.Sandhi.Types
import Phonology.Sandhi.Boundary as Boundary


{-
    helper function for applySandhiToBoundaries

    input:  - old trace as String
            - new trace as String
    output: extended trace of form "trace1 | trace2 | trace3"
-}
extendTrace :: String -> String -> String
extendTrace ""  new = new
extendTrace old new = old ++ " | " ++ new

{-
    apply word-boundary over sentence words

    input:  words in sentence as [String]
    output: list of sandhi results as (sandhita, trace)
-}
applySandhiToBoundaries :: [String] -> [(String, String)]
applySandhiToBoundaries sentenceWords =
    reverse (apply sentenceWords Nothing []) where
    apply :: [String]                 -- words remaining in sentence
          -> Maybe (String, String)   -- previous successful sandhi result (Nothing if unsuccessful)
          -> [(String, String)]       -- (sandhita, trace) accumulator
          -> [(String, String)]       -- resulting (sandhita, trace) tuples
    -- base case for empty sentence
    apply [] Nothing sandhiResultAcc =
        sandhiResultAcc
    apply [] (Just prevSandhiResult) sandhiResultAcc =
        prevSandhiResult : sandhiResultAcc
    apply (firstWord : restWords) Nothing sandhiResultAcc =
        apply restWords (Just (firstWord, "")) sandhiResultAcc
    apply (firstWord : restWords) (Just (prevSandhita, prevTrace)) sandhiResultAcc =
        case Boundary.applyBoundarySandhi (prevSandhita, firstWord) of
            -- consecutive successful sandhi, keep accumulating
            -- do NOT commit result to acc YET
            Sandhita extendedSandhita newTrace ->
                let extendedTrace = extendTrace prevTrace newTrace
                    extendedResult = (extendedSandhita, extendedTrace)
                in
                    apply restWords (Just extendedResult) sandhiResultAcc
            -- previous successful sandhi just ended
            Unchanged _ ->
                let prevResult = (prevSandhita, prevTrace) in
                apply restWords (Just (firstWord, "")) (prevResult : sandhiResultAcc)

{- word-internal sandhi happens AFTER word boundary sandhi

    applySandhi, as the simplest-named function, must serve as the
    singular (String -> String) sandhi applicator.

    therefore, word boundary sandhi will be applied to a sentence's words first,
    then the sandhitas and words will each undergo word-internal, or a secret
    more general third thing, that will dictate a more "everywhere" kind of
    phonological sandhi.

    to symbolize being a "more general" type of sandhi that gets applied AFTER
    a more specific axis, namely word-boundary sandhi, there will be no "word/"
    qualifier in its trace
-}

cleanUpSandhiOutput :: [(String, String)] -> [String]
cleanUpSandhiOutput [] = []
cleanUpSandhiOutput ((sandhita, "") : rest) =
    sandhita : cleanUpSandhiOutput rest
cleanUpSandhiOutput ((sandhita, trace) : rest) =
    (sandhita ++ " (" ++ trace ++ ")") : cleanUpSandhiOutput rest

{-
    entrypoint for sandhi application

    input:  sentence as String
    output: sandhified sentence as String
-}
applySandhi :: String -> String
applySandhi sentence =
    let sandhiOutput = applySandhiToBoundaries (Prelude.words sentence) in
    Prelude.unwords (cleanUpSandhiOutput sandhiOutput)

encodeSandhiResult :: String -> String -> String
encodeSandhiResult sentence output =
    "(sandhi)" ++ "\n"
    ++  "sentence:  "   ++ sentence ++ "\n"
    ++  "output:    "   ++ output

runSandhi :: IO String
runSandhi = do
    putStrLn ""
    putStrLn "input sentence:"
    sentence <- getLine
    let output = applySandhi sentence
    putStrLn ""
    putStrLn "sandhi output:"
    putStrLn output
    return (encodeSandhiResult sentence output)
