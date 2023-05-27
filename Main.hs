module Main where

import Data.Char (toLower)
import Data.List (foldr)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Wordle
import WordleGuesser (guesser)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  guesses <- lines <$> readFile "wordlists/wordle_guesses.txt"
  answers <- lines <$> readFile "wordlists/wordle_answers.txt"
  putStrLn "You have 6 options:"
  putStrLn " 1) Watch WordleGuesser guess a random Wordle word"
  putStrLn " 2) Watch WordleGuesser guess a Wordle word of your choosing"
  putStrLn " 3) Compete against WordleGuesser to guess a random Wordle word the fastest!"
  putStrLn " 4) Watch WordleGuesser guess 10 random Wordle words, and see the average score"
  putStrLn " 5) Test WordleGuesser on all 2315 Wordle words (takes a long time)"
  putStrLn " 6) Play a standard game of Wordle yourself"
  putStr "Enter your chosen number: "
  input <- getLine
  case input of
    "1" -> do
      playWordleBot guesses answers guesser Nothing
      return ()
    "2" -> do
      putStr "Enter a word for WordleGuesser to guess: "
      answer <- getLine
      playWordleBot guesses answers guesser (Just $ map toLower answer)
      return ()
    "3" -> do
      answer <- getRandomAnswer answers
      putStrLn "Let's start with the human:"
      humanScore <- playWordle guesses answers $ Just answer
      putStrLn "Now it's the machine's turn:"
      machineScore <- playWordleBot guesses answers guesser $ Just answer
      if humanScore < machineScore
        then do
          putStrLn "Congratulations you win!"
        else
          if humanScore == machineScore
            then do
              putStrLn "It's a tie..."
            else do
              putStrLn "The machine wins, unlucky."
    "4" -> do
      let n = 10
      let scores = [playWordleBot guesses answers guesser Nothing | _ <- [1 .. n]]
      total <- foldr sumIO (return 0) scores
      let avg_score = fromIntegral total / fromIntegral n
      putStrLn $ "Average score: " ++ show avg_score
    "5" -> do
      putStrLn "Please wait..."
      let scores = testWordleBot guesses answers guesser
      let avg_score = fromIntegral (sum scores) / fromIntegral (length scores)
      putStrLn $ "Average score: " ++ show avg_score
      print scores
    "6" -> do
      _ <- playWordle guesses answers Nothing
      return ()
    _ -> do
      putStrLn "Invalid option"
      main
  putStr "Hit enter to do something else: "
  _ <- getLine
  putStrLn "\n"
  main

sumIO :: IO Int -> IO Int -> IO Int
sumIO a b = do
  x <- a
  y <- b
  return $ x + y
