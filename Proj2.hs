module Proj2
  ( Pitch,
    toPitch,
    feedback,
    GameState,
    initialGuess,
    nextGuess,
    Chord,
    Feedback,
    chords,
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

chords =
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

feedback :: Chord -> Chord -> Feedback
feedback target guess = (samePitch, sameNote - samePitch, sameOct - samePitch)
  where
    samePitch = countCommon target guess
    (targetNotes, targetOcts) = splitChord target
    (guessNotes, guessOcts) = splitChord guess
    sameNote = countCommon targetNotes guessNotes
    sameOct = countCommon targetOcts guessOcts

countCommon :: Eq a => [a] -> [a] -> Int
countCommon (x : xs) ys
  | x `elem` ys = 1 + countCommon xs (delete x ys)
  | otherwise = countCommon xs ys
countCommon [] _ = 0

splitChord :: Chord -> ([Char], [Int])
splitChord [] = ([], [])
splitChord (Pitch note oct : tail) = (note : notes, oct : octs)
  where
    (notes, octs) = splitChord tail

initialGuess :: (Chord, GameState)
initialGuess = (guess, GameState (delete guess chords))
  where
    guess = map (fromJust . toPitch) ["G2", "E3", "F3"]

nextGuess :: (Chord, GameState) -> Feedback -> (Chord, GameState)
nextGuess (guess, GameState options) yourFeedback =
  (nextGuess, GameState (delete nextGuess newOptions))
  where
    newOptions = filter ((== yourFeedback) . feedback guess) options
    metric = guessScore newOptions
    nextGuess = maximumBy (comparing metric) newOptions

guessScore :: [Chord] -> Chord -> Int
guessScore options guess = - maximum (counts (map (feedback guess) options))

counts :: Ord a => [a] -> [Int]
counts = map length . group . sort
