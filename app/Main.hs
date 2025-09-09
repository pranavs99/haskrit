module Main where

import Phonology.Phoneme


run :: Show a => String -> (String -> Either String a) -> String -> IO ()
run action f input = case f input of
    Right result    -> print result
    Left err        -> putStrLn ("error in " ++ action ++ ": " ++ err)


main :: IO ()
main = do
    putStrLn "enter input:"
    word <- getLine
    run "phoneme collection" phonemeNames word
