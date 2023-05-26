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

import Data.Char (chr)
import Data.List (all)

type Answer = String

type Guess = String

data LetterFeedback = Correct | WrongPosition | Wrong
  deriving (Eq, Ord)

instance Show LetterFeedback where
  show c = case c of
    Correct -> [chr 129001]
    WrongPosition -> [chr 129000]
    Wrong -> [chr 128997]

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

getFeedback :: Guess -> Answer -> GuessFeedback
getFeedback guess answer =
  let (guess', answer', areCorrect) = filterCorrect guess answer
      areWrongPosition = getWrongPosition guess' answer'
   in knitFeedbacks areCorrect areWrongPosition

knitFeedbacks :: [Bool] -> [Bool] -> GuessFeedback
knitFeedbacks (c : cs) (wp : wps) =
  case (c, wp) of
    (True, _) -> Correct : knitFeedbacks cs (wp : wps)
    (_, True) -> WrongPosition : knitFeedbacks cs wps
    _ -> Wrong : knitFeedbacks cs wps
knitFeedbacks cs _ = map (\c -> if c then Correct else Wrong) cs

filterCorrect :: Guess -> Answer -> (String, String, [Bool])
filterCorrect (g : gs) (a : as)
  | g == a = (gs', as', True : correct')
  | otherwise = (g : gs', a : as', False : correct')
  where
    (gs', as', correct') = filterCorrect gs as
filterCorrect _ _ = ("", "", [])

getWrongPosition :: Guess -> Answer -> [Bool]
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
