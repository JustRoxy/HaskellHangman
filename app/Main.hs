module Main where

import Hangman
import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)

playGame :: Quiz -> IO ()
playGame q@(Quiz w ans hp) = do 
    putStrLn ("Your hp is " ++ show hp ++ " and your answers are " ++ show ans ++ ", the word is " ++ showBlured q)
    next <- getLine
    case addAnswer (head next) q of
        (Pair _ Won) -> do
            putStrLn ("You won! The word was " ++ w)
            exitSuccess
        (Pair g Quess) -> do
            putStrLn "Right!"
            playGame g

        (Pair g AlreadySaid) -> do
            putStrLn ("Not unique answer (" ++ [head next] ++ "): ")
            playGame g

        (Pair _ Lost) -> do
            putStrLn ("You lost, the word was \"" ++ w ++ "\"")
            exitFailure

        (Pair g Nope) -> do
            putStrLn "Wrong guess"
            playGame g

main :: IO ()
main = do
    word <- getArgs
    case word of
        [] -> exitFailure
        _ -> playGame $ newGame $ head word