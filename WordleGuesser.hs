{-
    Each guess, we trim down the set of remaining answers to the ones
    consistent with all of the feedback given so far, and we track these
    remaining answers as the current game state. A potential guess is evaluated
    by calculating all the feedbacks that could be received from the remaining
    answers. We then calulate the entropy of these feedbacks, to provide a
    score for this potential guess. Higher entropy is desirable as it means the
    feedbacks are more evenly distributed, so the true feedback we receive
    contains high information and is likely to cut down the remaining answers
    as much as possible.

    For the very first guess, we calculated and hardcoded the guess that has
    the highest entropy to save computing time. After this, we repeatedly
    filter the remaining answers and select the highest entropy guess until we
    find the target.
-}

module WordleGuesser (guesser) where

import Data.List (delete, group, maximumBy, sort, subsequences)
import Data.Ord (comparing)
import Wordle

type State = ([Guess], [Answer])

guesser :: Guesser State
guesser = Guesser initialGuess getNextGuess

-- | Called to generate the first guess and corresponding state that is passed to
-- future calls. Takes a list of possible guesses and answers to setup game.
initialGuess :: [Guess] -> [Answer] -> (Guess, State)
initialGuess guesses answers = (guess, (guesses, answers))
  where
    guess = "soare"

-- | Takes a tuple containing the guessed guess and remaining answers from the
-- previous turn, as well as the feedback given from this guess. The answers
-- are filtered down to those consistent with the feedback, and the getNextGuess
-- is selected from the filtered answers using maxEntropyGuess.
getNextGuess :: (Guess, State) -> GuessFeedback -> (Guess, State)
getNextGuess (oldGuess, (oldPotentialGuesses, oldPotentialAnswers)) feedback =
  (newGuess, (potentialGuesses, potentialAnswers))
  where
    potentialGuesses = delete oldGuess oldPotentialGuesses
    potentialAnswers' = delete oldGuess oldPotentialAnswers
    potentialAnswers = consistentAnswers feedback oldGuess potentialAnswers'
    newGuess = maxEntropyGuess potentialGuesses potentialAnswers

-- | Takes a feedback, the guess that generated the feedback and a list of
-- answers. Returns a filtered version of the list of answers, containing only
-- those that generate the same feedback for the given guess.
consistentAnswers :: GuessFeedback -> Guess -> [Answer] -> [Answer]
consistentAnswers feedback guess = filter ((== feedback) . getFeedback guess)

-- | Takes a list of potential guesses and answers. Returns the guess which
-- produces the highest entropy among the answers according to 'guessEntropy'.
maxEntropyGuess :: [Guess] -> [Answer] -> Answer
maxEntropyGuess guesses answers = maximumBy (comparing (guessEntropy answers)) guesses

-- | Takes a list of answers and a potential guess. Calculates all the
-- feedbacks that could be received from the answers if that guess was made,
-- and returns the entropy of that feedback.
guessEntropy :: [Answer] -> Guess -> Double
guessEntropy answers guess = entropy potentialFeedbacks + bonus
  where
    potentialFeedbacks = map (getFeedback guess) answers
    bonus = if guess `elem` answers then 0.001 else 0

-- | Takes a list containing elements of an ordered type. Returns Shannon's
-- entropy of the list in bits.
-- >>> entropy [0..255]
-- 8.0
entropy :: Ord a => [a] -> Double
entropy xs = sum [- p * logBase 2 p | p <- elementDistribution xs]

-- | Takes a list containing elements of an ordered type. For each distinct
-- element, calculates what proportion of the list that element makes up.
-- Returns these proportions as a list of doubles, which will sum to 1.
elementDistribution :: Ord a => [a] -> [Double]
elementDistribution xs = [freq `floatDiv` length xs | freq <- frequencies xs]
  where
    floatDiv x y = fromIntegral x / fromIntegral y

-- | Takes a list containing elements of an ordered type. Returns a list of
-- ints counting the number of occurences of each distinct element.
frequencies :: Ord a => [a] -> [Int]
frequencies = map length . group . sort
