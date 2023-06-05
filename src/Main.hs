module Main where

import Control.Monad (zipWithM_)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Safe (atMay)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Text.Read (readMaybe)
import Wordle (cheatWordleBot, getRandomAnswer, playWordle, playWordleBot)
import WordleGuesser (guesser)

data Action = Action {summary :: String, run :: [String] -> [String] -> IO Int}

actions :: [Action]
actions =
  [ Action
      "Watch WordleGuesser guess a random Wordle word"
      (\guesses answers -> playWordleBot guesses answers guesser Nothing),
    Action
      "Watch WordleGuesser guess a Wordle word of your choosing"
      ( \guesses answers -> do
          putStr "Enter a word for WordleGuesser to guess: "
          answer <- getLine
          playWordleBot guesses answers guesser (Just $ map toLower answer)
      ),
    Action
      "Compete against WordleGuesser to guess a random Wordle word the fastest"
      ( \guesses answers -> do
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
          return 0
      ),
    Action
      "Use WordleGuesser to cheat"
      ( \guesses answers -> do
          cheatWordleBot guesses answers guesser
          return 0
      ),
    Action
      "Play a standard game of Wordle yourself"
      (\guesses answers -> playWordle guesses answers Nothing)
  ]

summariseAction :: Action -> Int -> IO ()
summariseAction (Action summary _) i = putStrLn $ " " ++ show i ++ ") " ++ summary

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  guesses <- lines <$> readFile "wordlists/wordle_guesses.txt"
  answers <- lines <$> readFile "wordlists/wordle_answers.txt"
  putStrLn $ "You have " ++ (show . length) actions ++ " options"
  zipWithM_ summariseAction actions [0 ..]
  putStr "Enter your chosen number: "
  input <- getLine
  case readMaybe input >>= atMay actions of
    Just (Action _ run) -> do
      putStrLn ""
      run guesses answers
      return ()
    Nothing -> do
      putStrLn $ "Invalid option, enter a number between 0 and " ++ show (length actions - 1)
  putStrLn "\n"
  main
