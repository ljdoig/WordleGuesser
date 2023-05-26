module Main where

import Data.Char (toLower)
import Data.List (foldr)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Random
import Wordle
import WordleGuesser (guesser)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  guesses <- lines <$> readFile "wordlists/wordle_guesses.txt"
  answers <- lines <$> readFile "wordlists/wordle_answers.txt"
  putStrLn "You have 4 options:"
  putStrLn " 1) Watch WordleGuesser guess a random Wordle word"
  putStrLn " 2) Watch WordleGuesser guess a Wordle word of your choosing"
  putStrLn " 3) Watch WordleGuesser guess 100 random Wordle words, and see the average score"
  putStrLn " 4) Play a standard game of Wordle yourself"
  putStr "Enter your chosen number: "
  input <- getLine
  case input of
    "1" -> do
      playWordleBotRandom guesses answers guesser
      return ()
    "2" -> do
      putStr "Enter a word for WordleGuesser to guess: "
      answer <- getLine
      playWordleBot guesses answers (map toLower answer) guesser
      return ()
    "3" -> do
      let n = 100
      let scores = [playWordleBotRandom guesses answers guesser | _ <- [1 .. n]]
      total <- foldr sumIO (return 0) scores
      let avg_score = fromIntegral total / fromIntegral n
      putStrLn "Please wait 10-20s"
      putStrLn $ "Average score: " ++ show avg_score
    "4" -> do
      playWordle guesses answers
      return ()
    _  -> do
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

test :: IO ()
test = do
  hSetBuffering stdout NoBuffering
  guesses <- lines <$> readFile "wordlists/wordle_guesses.txt"
  answers <- lines <$> readFile "wordlists/wordle_answers.txt"
  let n = 100
  rands <- randomList n 0 $ length answers
  let scores = [testWordleBot guesses answers (answers !! i) guesser | i <- rands]
  let avg_score = fromIntegral (sum scores) / fromIntegral n
  putStrLn $ "Average score: " ++ show avg_score

randomList :: Int -> Int -> Int -> IO [Int]
randomList n lower upper =
  do
    gen <- getStdGen
    let rs = (randomRs (lower, upper) gen :: [Int])
    return (take n rs)
