module Main where

import Hangman
import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)

playGame :: Quiz -> IO ()
playGame q@(Quiz w ans hp) = do 
    putStrLn ("Your hp is " ++ show hp ++ " and your answers are " ++ show ans ++ ", the word is " ++ showBlured q)
    next <- getLine
    case addAnswer (head next) q of
        (_, Win) -> do
            putStrLn ("You won! The word was " ++ w)
            exitSuccess
        (g, Quess) -> do
            putStrLn "Right!"
            playGame g

        (g, AlreadySaid) -> do
            putStrLn ("Not unique answer (" ++ [head next] ++ "): ")
            playGame g

        (_, Lost) -> do
            putStrLn ("You lost, the word was \"" ++ w ++ "\"")
            exitFailure

        (g, Nope) -> do
            putStrLn "Wrong guest"
            playGame g

main :: IO ()
main = do
    word <- getArgs
    case word of
        [] -> exitFailure
        _ -> playGame $ newGame $ head word