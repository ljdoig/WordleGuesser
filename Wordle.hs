module Wordle
  ( Answer,
    Guess,
    LetterFeedback,
    GuessFeedback,
    Guesser (Guesser),
    numAllowedGuesses,
    hasWon,
    getFeedback,
  )
where

import Data.List (all)

type Answer = String

type Guess = String

data LetterFeedback = Correct | WrongPosition | Wrong
  deriving (Eq, Show, Ord)

type GuessFeedback = [LetterFeedback]

data Guesser state = Guesser
  { initialGuess :: [Answer] -> [Guess] -> (Guess, state),
    getNextGuess :: (Guess, state) -> GuessFeedback -> (Guess, state)
  }

numAllowedGuesses :: Int
numAllowedGuesses = 6

wordLength :: Int
wordLength = 5

hasWon :: GuessFeedback -> Bool
hasWon = all (== Correct)

getFeedback :: Answer -> Guess -> GuessFeedback
getFeedback _ _ = replicate 5 Correct
