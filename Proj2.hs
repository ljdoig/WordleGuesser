module Proj2
  ( Pitch,
    toPitch,
    feedback,
    GameState,
    initialGuess,
    nextGuess,
  )
where

import Data.List (delete, group, maximumBy, sort, subsequences)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Text.Read (readMaybe)

data Note = A | B | C | D | E | F | G
  deriving (Eq, Read, Show, Enum, Bounded)

data Octave = O1 | O2 | O3
  deriving (Eq, Read, Show, Enum, Bounded)

data Pitch = Pitch Note Octave
  deriving (Eq)

instance Show Pitch where show (Pitch note oct) = show note ++ tail (show oct)

newtype GameState = GameState [Chord]

type Chord = [Pitch]

type Feedback = (Int, Int, Int)

allPitches :: [Pitch]
allPitches = [Pitch note octave | note <- [minBound ..], octave <- [minBound ..]]

allChords :: [Chord]
allChords = filter ((== 3) . length) (subsequences allPitches)

toPitch :: String -> Maybe Pitch
toPitch pitchStr =
  let (noteStr, octaveStr) = splitAt 1 pitchStr
   in case (readMaybe noteStr, readMaybe ('O' : octaveStr)) of
        (Just note, Just octave) -> Just (Pitch note octave)
        _ -> Nothing

feedback :: Chord -> Chord -> Feedback
feedback guess target =
  (numSamePitch, numSameNote - numSamePitch, numSameOctave - numSamePitch)
  where
    numSamePitch = numCommonElements target guess
    (targetNotes, targetOctaves) = splitChord target
    (guessNotes, guessOctaves) = splitChord guess
    numSameNote = numCommonElements targetNotes guessNotes
    numSameOctave = numCommonElements targetOctaves guessOctaves

numCommonElements :: Eq a => [a] -> [a] -> Int
numCommonElements [] _ = 0
numCommonElements (x : xs) ys
  | x `elem` ys = 1 + numCommonElements xs (delete x ys)
  | otherwise = numCommonElements xs ys

splitChord :: Chord -> ([Note], [Octave])
splitChord [Pitch n1 o1, Pitch n2 o2, Pitch n3 o3] = ([n1, n2, n3], [o1, o2, o3])
splitChord _ = error "invalid chord"

initialGuess :: (Chord, GameState)
initialGuess = (guess, GameState (delete guess allChords))
  where
    guess = map (fromJust . toPitch) ["E2", "F3", "G3"]

nextGuess :: (Chord, GameState) -> Feedback -> (Chord, GameState)
nextGuess (oldGuess, GameState oldTargets) yourFeedback =
  (newGuess, GameState (delete newGuess newTargets))
  where
    newTargets = consistentTargets yourFeedback oldGuess oldTargets
    newGuess = maxEntropyGuess newTargets

consistentTargets :: Feedback -> Chord -> [Chord] -> [Chord]
consistentTargets yourFeedback guess =
  filter ((== yourFeedback) . feedback guess)

maxEntropyGuess :: [Chord] -> Chord
maxEntropyGuess targets = maximumBy (comparing (guessEntropy targets)) targets

guessEntropy :: [Chord] -> Chord -> Double
guessEntropy targets guess = entropy potentialFeedbacks
  where
    potentialFeedbacks = map (feedback guess) targets

entropy :: Ord a => [a] -> Double
entropy xs = sum [- p * log p | p <- normalisedFrequencies xs]

normalisedFrequencies :: Ord a => [a] -> [Double]
normalisedFrequencies xs = [freq `floatDiv` length xs | freq <- frequencies xs]
  where
    floatDiv x y = fromIntegral x / fromIntegral y

frequencies :: Ord a => [a] -> [Int]
frequencies = map length . group . sort
