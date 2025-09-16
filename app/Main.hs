module Main where

import Phonology.Sandhi


run :: String -> (String -> String) -> String -> IO ()
run action f input = let result = f input in
    do
        putStrLn ("action: " ++ action)
        putStrLn ("input: " ++ input)
        putStrLn ("result: " ++ result)


main :: IO ()
main = runSandhi
