module Wordle
  ( Guess,
    Answer,
    GuessFeedback,
    Guesser (Guesser),
    getFeedback,
    playWordle,
    playWordleBotRandom,
    playWordleBot,
    testWordleBot,
  )
where

import Data.Char (toUpper)
import Data.List (all)
import System.Random (randomIO)

type Answer = String

type Guess = String

data LetterFeedback = Correct | WrongPosition | Wrong
  deriving (Eq, Ord)

type GuessFeedback = [LetterFeedback]

data Guesser guesserState = Guesser
  { initialGuess :: [Answer] -> [Guess] -> (Guess, guesserState),
    getNextGuess :: GetNextGuess guesserState
  }

type GetNextGuess guesserState = (Guess, guesserState) -> GuessFeedback -> (Guess, guesserState)

numAllowedGuesses = 6

playWordle :: [Guess] -> [Answer] -> IO ()
playWordle guesses answers = do
  randInt <- randomIO :: IO Int
  let answer = answers !! mod randInt (length answers)
  playWordleTurn guesses answer 1

playWordleTurn :: [Guess] -> Answer -> Int -> IO ()
playWordleTurn guesses answer n = do
  putStr $ "Enter guess #" ++ show n ++ ": "
  guess <- getLine
  if guess `notElem` guesses
    then do
      putStrLn "Invalid guess, try again"
      playWordleTurn guesses answer n
    else do
      let feedback = getFeedback guess answer
      printColoured guess feedback
      if hasWon feedback
        then do
          putStrLn $ "You got it in " ++ show n ++ " guesses!"
        else do
          if n == numAllowedGuesses
            then do
              putStrLn "You didn't get it..."
              printPlain answer
            else do
              playWordleTurn guesses answer (n + 1)

failurePenalty = 4

playWordleBot :: [Guess] -> [Answer] -> Answer -> Guesser guesserState -> IO Int
playWordleBot guesses answers answer guesser =
  if answer `elem` answers
    then beginWordleBotGame guesses answers answer guesser
    else do
      putStrLn "Not a valid Wordle answer, defaulting to a random choice"
      playWordleBotRandom guesses answers guesser

playWordleBotRandom :: [Guess] -> [Answer] -> Guesser guesserState -> IO Int
playWordleBotRandom guesses answers guesser = do
  randInt <- randomIO :: IO Int
  let answer = answers !! mod randInt (length answers)
  beginWordleBotGame guesses answers answer guesser

beginWordleBotGame :: [Guess] -> [Answer] -> Answer -> Guesser guesserState -> IO Int
beginWordleBotGame guesses answers answer (Guesser initialGuess getNextGuess) = do
  let (guess, state) = initialGuess guesses answers
  putStrLn "Beginning the Wordle game!"
  wordleBotTurn answer getNextGuess (guess, state) 1

wordleBotTurn :: Answer -> GetNextGuess guesserState -> (Guess, guesserState) -> Int -> IO Int
wordleBotTurn answer getNextGuess (guess, guesserState) n = do
  let feedback = getFeedback guess answer
  printColoured guess feedback
  if hasWon feedback
    then do
      putStrLn $ "WordleGuesser got it in " ++ show n ++ " guesses!"
      return n
    else do
      if n == numAllowedGuesses
        then do
          putStrLn "WordleGuesser didn't get it..."
          printPlain answer
          return $ n + failurePenalty
        else do
          let (guess', guesserState') = getNextGuess (guess, guesserState) feedback
          wordleBotTurn answer getNextGuess (guess', guesserState') (n + 1)

testWordleBot :: [Guess] -> [Answer] -> Answer -> Guesser guesserState -> Int
testWordleBot guesses answers answer (Guesser initialGuess getNextGuess) =
  let (guess, state) = initialGuess guesses answers
   in testWordleBotTurn answer getNextGuess (guess, state) 1

testWordleBotTurn :: Answer -> GetNextGuess guesserState -> (Guess, guesserState) -> Int -> Int
testWordleBotTurn answer getNextGuess (guess, guesserState) n = do
  let feedback = getFeedback guess answer
  if hasWon feedback
    then n
    else
      if n == numAllowedGuesses
        then n + failurePenalty
        else do
          let (guess', guesserState') = getNextGuess (guess, guesserState) feedback
          testWordleBotTurn answer getNextGuess (guess', guesserState') (n + 1)

printColoured :: Guess -> GuessFeedback -> IO ()
printColoured (c : cs) (f : fs) = do
  case f of
    Correct -> putStr $ "\ESC[32m " ++ [toUpper c]
    WrongPosition -> putStr $ "\ESC[33m " ++ [toUpper c]
    Wrong -> putStr $ "\ESC[31m " ++ [toUpper c]
  printColoured cs fs
printColoured _ _ = putStrLn "\ESC[0m"

printPlain :: Answer -> IO ()
printPlain answer = putStrLn $ "The answer was:\n" ++ concatMap (\c -> [' ', toUpper c]) answer

hasWon :: GuessFeedback -> Bool
hasWon = all (== Correct)

getFeedback :: Guess -> Answer -> GuessFeedback
getFeedback guess answer =
  let (guess', answer', areCorrect) = filterCorrect guess answer
      areWrongPosition = getWrongPosition guess' answer'
   in knitFeedbacks areCorrect areWrongPosition

knitFeedbacks :: [Bool] -> [Bool] -> [LetterFeedback]
knitFeedbacks (c : cs) (wp : wps) =
  case (c, wp) of
    (True, _) -> Correct : knitFeedbacks cs (wp : wps)
    (_, True) -> WrongPosition : knitFeedbacks cs wps
    _ -> Wrong : knitFeedbacks cs wps
knitFeedbacks cs _ = map (\c -> if c then Correct else Wrong) cs

filterCorrect :: String -> String -> (String, String, [Bool])
filterCorrect (g : gs) (a : as)
  | g == a = (gs', as', True : correct')
  | otherwise = (g : gs', a : as', False : correct')
  where
    (gs', as', correct') = filterCorrect gs as
filterCorrect _ _ = ("", "", [])

getWrongPosition :: String -> String -> [Bool]
getWrongPosition (g : gs) as =
  let (as', isPresent) = removeIfPresent g as
      areWrongPosition' = getWrongPosition gs as'
   in (isPresent : areWrongPosition')
getWrongPosition _ _ = []

removeIfPresent :: Char -> String -> (String, Bool)
removeIfPresent c (x : xs)
  | c == x = (xs, True)
  | otherwise =
    let (xs', isPresent) = removeIfPresent c xs
     in (x : xs', isPresent)
removeIfPresent _ _ = ([], False)
