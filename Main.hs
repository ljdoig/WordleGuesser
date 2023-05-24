module Main where

import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Wordle
import WordleGuesser (guesser)

type GetNextGuess state = ((Guess, state) -> GuessFeedback -> (Guess, state))

-- | Given the correct answer, a guess, the guessers game state and a guess number,
--   continue until the right answer is guessed or 6 guesses have been made.
loop :: Answer -> GetNextGuess state -> Guess -> state -> Int -> IO ()
loop answer getNextGuess guess gameState guessNum = do
  putStrLn $ "Guess #" ++ show guessNum ++ ":  " ++ guess
  let feedback = getFeedback answer guess
  putStrLn $ "Feedback:  " ++ show feedback
  if hasWon feedback
    then do
      putStrLn $ "WordleGuesser got it in " ++ show guessNum ++ " guesses!"
    else
      if guessNum == numAllowedGuesses
        then do
          putStrLn "WordleGuesser didn't get it..."
        else do
          let (guess', gameState') = getNextGuess (guess, gameState) feedback
          loop answer getNextGuess guess' gameState' (guessNum + 1)

runWordleGuesser :: [Answer] -> [Guess] -> IO ()
runWordleGuesser answers guesses = do
  putStrLn "What word would you like the WordleGuesser to guess?"
  answer <- getLine
  if answer `elem` answers
    then do
      let Guesser initialGuess getNextGuess = guesser
      let (guess, gameState) = initialGuess answers guesses
      loop answer getNextGuess guess gameState 1
    else do
      putStrLn "Invalid answer, try again"
      runWordleGuesser answers guesses

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  runWordleGuesser [] []
