module Proj2
  ( Pitch,
    toPitch,
    feedback,
    GameState,
    initialGuess,
    nextGuess,
    Chord,
    Feedback,
    allChords,
  )
where

import Data.Char (digitToInt)
import Data.List (delete, group, maximumBy, sort)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

data Pitch = Pitch Char Int
  deriving (Eq, Ord)

instance Show Pitch where
  show (Pitch note oct) = [note, head (show oct)]

type Chord = [Pitch]

newtype GameState = GameState [Chord]

type Feedback = (Int, Int, Int)

notes = ['A' .. 'G']

octs = [1 .. 3]

pitches = [Pitch n o | o <- octs, n <- notes]

allChords =
  [ [pitches !! i, pitches !! j, pitches !! k]
    | i <- [0 .. length pitches - 1],
      j <- [i + 1 .. length pitches - 1],
      k <- [j + 1 .. length pitches - 1]
  ]

toPitch :: String -> Maybe Pitch
toPitch [note, octChar]
  | note `elem` notes && oct `elem` octs = Just (Pitch note oct)
  | otherwise = Nothing
  where
    oct = digitToInt octChar
toPitch _ = Nothing

toChord :: String -> [Pitch]
toChord = fromJust . mapM toPitch . words

feedback :: Chord -> Chord -> Feedback
feedback target guess = (samePitch, sameNote - samePitch, sameOct - samePitch)
  where
    samePitch = countCommon target guess
    (targetNotes, targetOcts) = splitChord target
    (guessNotes, guessOcts) = splitChord guess
    sameNote = countCommon targetNotes guessNotes
    sameOct = countCommon targetOcts guessOcts

countCommon :: Eq a => [a] -> [a] -> Int
countCommon [] _ = 0
countCommon (x : xs) ys
  | x `elem` ys = 1 + countCommon xs (delete x ys)
  | otherwise = countCommon xs ys

splitChord :: Chord -> ([Char], [Int])
splitChord [Pitch n1 o1, Pitch n2 o2, Pitch n3 o3] = ([n1, n2, n3], [o1, o2, o3])
splitChord _ = error "invalid chord"

initialGuess :: (Chord, GameState)
initialGuess = (guess, GameState (delete guess allChords))
  where
    guess = toChord "G2 E3 F3"

nextGuess :: (Chord, GameState) -> Feedback -> (Chord, GameState)
nextGuess (guess, GameState targets) yourFeedback =
  (nextGuess, GameState (delete nextGuess remainingTargets))
  where
    remainingTargets = filter ((== yourFeedback) . feedback guess) targets
    scoreOrdering = comparing (guessScore remainingTargets)
    nextGuess = maximumBy scoreOrdering remainingTargets

guessScore :: [Chord] -> Chord -> Int
guessScore targets guess = - sumOfSquares (countDistinct potentialFeedbacks)
  where
    potentialFeedbacks = map (feedback guess) targets
    countDistinct = map length . group . sort
    sumOfSquares = sum . map (^ 2)
