{-
    Each guess, we trim down the set of remaining targets to the ones
    consistent with all of the feedback given so far, and we track these
    remaining targets as the current game state. A potential guess is evaluated
    by calculating all the feedbacks that could be received from the remaining
    targets. We then calulate the entropy of these feedbacks, to provide a
    score for this potential guess. Higher entropy is desirable as it means the
    feedbacks are more evenly distributed, so the true feedback we receive
    contains high information and is likely to cut down the remaining targets
    as much as possible.

    For the very first guess, we calculated and hardcoded the guess that has
    the highest entropy to save computing time. After this, we repeatedly
    filter the remaining targets and select the highest entropy guess until we
    find the target.
-}

module WordleGuesser ( guesser )  where

import Data.List (delete, group, maximumBy, sort, subsequences)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Text.Read (readMaybe)
import Wordle

type State = [String]

guesser :: Guesser State
guesser = Guesser initialGuess getNextGuess

-- | A tuple containing the first guess guessed and the list of remaining
-- targets to guess in future. The guess was initially calculated using
-- 'maxEntropyGuess allGuesss', but was then hardcoded to save time.
initialGuess :: [Answer] -> [Guess] -> (Guess, State)
initialGuess _ guesses = (guess, delete guess guesses)
  where
    guess = "crate"

-- | Takes a tuple containing the guessed guess and remaining targets from the
-- previous turn, as well as the feedback given from this guess. The targets
-- are filtered down to those consistent with the feedback, and the getNextGuess
-- is selected from the filtered targets using maxEntropyGuess.
getNextGuess :: (Guess, State) -> GuessFeedback -> (Guess, State)
getNextGuess (oldGuess, oldTargets) feedback =
  (newGuess, delete newGuess newTargets)
  where
    newTargets = consistentTargets feedback oldGuess oldTargets
    newGuess = maxEntropyGuess newTargets

-- | Takes a feedback, the guess that generated the feedback and a list of
-- targets. Returns a filtered version of the list of targets, containing only
-- those that generate the same feedback for the given guess.
consistentTargets :: GuessFeedback -> Guess -> [Guess] -> [Guess]
consistentTargets feedback guess =
  filter ((== feedback) . getFeedback guess)

-- | Takes a list of targets. Returns the one with the highest 'guessEntropy'.
maxEntropyGuess :: [Guess] -> Guess
maxEntropyGuess targets = maximumBy (comparing (guessEntropy targets)) targets

-- | Takes a list of targets and a potential guess. Calculates all the
-- feedbacks that could be received from the targets if that guess was made,
-- and returns the entropy of that feedback.
guessEntropy :: [Guess] -> Guess -> Double
guessEntropy targets guess = entropy potentialFeedbacks
  where
    potentialFeedbacks = map (getFeedback guess) targets

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
