module Hangman where

type MakedWord = String
type Answers = String
type Health = Int

data Pair a b= Pair a b deriving (Eq, Show)

data Game = Won | Quess | AlreadySaid | Lost | Nope deriving (Eq, Show)
data Quiz = Quiz MakedWord Answers Health deriving (Eq, Show)


newGame :: MakedWord -> Quiz
newGame word = Quiz word [] 5 


checkAnswer :: Quiz -> Pair Quiz Game
checkAnswer q@(Quiz word ans _) = Pair q res
    where res 
            | all (`elem` ans) word = Won 
            | otherwise = Quess


showBlured :: Quiz -> String
showBlured (Quiz w a _) = map (\x -> if x `notElem` a then '*' else x) w


addAnswer :: Char -> Quiz -> Pair Quiz Game
addAnswer ch g@(Quiz word ans hp)
    | ch `elem` ans  = Pair g AlreadySaid 
    | ch `elem` word = checkAnswer (Quiz word (ch:ans) hp)
    | hp == 1        = Pair g Lost
    | otherwise      = Pair (Quiz word (ch:ans) (hp - 1)) Nope