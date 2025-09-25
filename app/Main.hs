module Main where

import Phonology.Sandhi
    (runSandhi)
import Phonology.Phoneme.Sutra
    (runMatchSutra)


runAction :: String -> Maybe (IO String)
runAction "sandhi"  = Just runSandhi
runAction "sutra"   = Just runMatchSutra
runAction _         = Nothing

printHistory :: [String] -> IO ()
printHistory [] = do
    putStrLn ""
    putStrLn "no history to print"
printHistory history = printOne history 1 where
    printOne :: [String] -> Int -> IO ()
    printOne [] _ = do
        putStrLn ""
        putStrLn "printHistory done: exiting"
    printOne (result : rest) i = do
        if i == 1 then
            putStrLn "\nsession history:\n"
        else
            putStrLn ""
        putStrLn "--------------------------------"
        putStrLn ("iteration " ++ show i)
        putStrLn result
        putStrLn "--------------------------------"
        printOne rest (i + 1)

main :: IO ()
main = loop [] where
    loop :: [String] -> IO ()
    loop history = do
        putStrLn ""
        putStrLn "action:"
        action <- getLine
        if action == "quit" then do
            printHistory (reverse history)
        else do
            case runAction action of
                Just f -> do
                    result <- f
                    loop (result : history)
                Nothing -> do
                    putStrLn ""
                    let msgUnrecognized = "(main) unrecognized action: " ++ action
                    putStrLn msgUnrecognized
                    loop (msgUnrecognized : history)
