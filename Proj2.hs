{-
    Author  : Lachlan Doig
    Purpose : Implements logic for a two-player logical guessing game, using an
              entropy-based approach for selecting the optimal guess

    The game of Musician is played between a composer and performer. The
    composer selects a chord made up of 3 distinct pitches, where each pitch 
    has a note (either A, B, C, D, E, F or G) and an octave (either 1, 2 or 3). 
    There are therefore 21 possible pitches (7 x 3) and 1330 possible chords
    (21 choose 3). The composer's chosen chord is the target, and the 
    performer's job is to find all pitches in the target in as few guesses as 
    possible. For each guess, the performer selects 3 distinct pitches and the 
    composer gives feedback about how close they are to the target, in the form 
    of 3 integers:

      1) the number of correct pitches (correct in both note and octave)
      2) the number of pitches with the correct note but an incorrect octave
      3) the number of pitches with the correct octave but an incorrect note

    For 2) and 3), repeated notes or octave in the guess will only be 
    considered correct if they are also repeated in the target. E.g. if you 
    guess A1, B1, C1 when the target is D1, E1, F2, then the feedback will be 
    (0, 0, 2), because the octave '1' appears only twice in the target. Pitches 
    counted in 1) are not counted in either 2) or 3). Some more examples:

    Target      Guess       Feedback
    A1,B2,C3    A1,B2,C1    2,1,0
    A1,B1,C1    A1,D1,E1    1,0,2
    A1,B1,C1    D1,E2,F3    0,0,1
    A1,A2,B3    A1,B1,B2    1,1,1

    This program implements the composer's answering logic to provide this 
    feedback but also the performer's guessing logic to deduce the target in a 
    small number of guesses. This approach first generates all 1330 possible 
    targets. Each guess, we trim down the set of remaining targets to the ones 
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

-- * Types used to represent pitches, chords, gamestate and feedback

-- | A note type, enumerating all possible notes in a pitch.
data Note = A | B | C | D | E | F | G
    deriving (Eq, Read, Show, Enum, Bounded)
  
-- | An octave type, enumerating all possible octaves in a pitch.
data Octave = O1 | O2 | O3
    deriving (Eq, Read, Show, Enum, Bounded)

-- | A pitch type. Pitches make up chords and are parsed using 'toPitch'.
data Pitch = Pitch Note Octave
    deriving Eq

-- | Pitches are shown as a two-character string, one each for note and octave.
-- >>> show (Pitch A O3)
-- "A3"
instance Show Pitch where show (Pitch note oct) = show note ++ tail (show oct)

-- | A chord type-synonym. A chord is a list of 3 pitches, but the length is 
-- not enforced by the type.
type Chord = [Pitch]

-- | A game-state type-synonym. Represents the list of chords that are still
-- possible based on all feedback given.
type GameState = [Chord]

-- | A feedback type-synonym. Feedback is given as a tuple of 3 integers, 
-- according to the rules described in the introduction.
type Feedback = (Int, Int, Int)


-- * Pitch parsing

-- | Takes a string describing a pitch. Returns the corresponding 'Just Pitch' 
-- for a 2-character string representing a valid pitch, and Nothing otherwise. 
-- A string represents a valid 'pitch' if it corresponds to the output of its
-- 'show pitch' described above, with one character each for note and octave.
-- >>> toPitch "A3"
-- Just A3
-- >>> toPitch "A4"
-- Nothing
toPitch :: String -> Maybe Pitch
toPitch [noteChar, octChar] = Pitch <$> maybeNote <*> maybeOct
    where maybeNote = readMaybe [noteChar] 
          maybeOct = readMaybe ['O', octChar]
toPitch _ = Nothing


-- * Composer logic for providing feedback
    
-- | Takes two chords, a guess and a target. Returns feedback according to the 
-- rules described in the introduction.
feedback :: Chord -> Chord -> Feedback
feedback guess target =
    (numSamePitch, numSameNote - numSamePitch, numSameOct - numSamePitch)
    where (targetNotes, targetOct) = splitChord target
          (guessNotes, guessOct) = splitChord guess
          numSamePitch = numCommonElements target guess
          numSameNote = numCommonElements targetNotes guessNotes
          numSameOct = numCommonElements targetOct guessOct

-- | Takes two lists whose elements can be compared for equality. Returns the 
-- number of elements that are common to both lists. Repeated elements are 
-- only counted if they are repeated in both lists.
-- >>> numCommonElements [1, 1, 1, 2, 2] [1, 2, 3]
-- 2
numCommonElements :: Eq a => [a] -> [a] -> Int
numCommonElements [] _ = 0
numCommonElements (x:xs) ys
    | x `elem` ys = 1 + numCommonElements xs (delete x ys)
    | otherwise = numCommonElements xs ys

-- | Takes a chord. Splits it into a list of notes and a list of octaves and
-- returns a tuple containing these lists.
splitChord :: Chord -> ([Note], [Octave])
splitChord = 
    foldr (\(Pitch note oct) (notes, octs) -> (note:notes, oct:octs)) ([], [])


-- * Performer logic for selecting an initial guess

-- | A list containing all possible pitches.
allPitches :: [Pitch]
allPitches = [Pitch note oct | note <- [minBound..], oct <- [minBound..]]

-- | A list with all possible chords. Initial GameState before first guess.
allChords :: GameState
allChords = filter ((== 3) . length) (subsequences allPitches)

-- | A tuple containing the first chord guessed and the list of remaining 
-- targets to guess in future. The guess was initially calculated using 
-- 'maxEntropyGuess allChords', but was then hardcoded to save time.
initialGuess :: (Chord, GameState)
initialGuess = (guess, delete guess allChords)
    where guess = map (fromJust . toPitch) ["E2", "F3", "G3"]


-- * Performer logic for selecting guesses from composer feedback

-- | Takes a tuple containing the guessed chord and remaining targets from the 
-- previous turn, as well as the feedback given from this guess. The targets 
-- are filtered down to those consistent with the feedback, and the nextGuess 
-- is selected from the filtered targets using maxEntropyGuess.
nextGuess :: (Chord, GameState) -> Feedback -> (Chord, GameState)
nextGuess (oldGuess, oldTargets) yourFeedback = 
    (newGuess, delete newGuess newTargets)
    where newTargets = consistentTargets yourFeedback oldGuess oldTargets
          newGuess = maxEntropyGuess newTargets

-- | Takes a feedback, the guess that generated the feedback and a list of 
-- targets. Returns a filtered version of the list of targets, containing only 
-- those that generate the same feedback for the given guess.
consistentTargets :: Feedback -> Chord -> [Chord] -> [Chord]
consistentTargets givenFeedback guess = 
    filter ((== givenFeedback) . feedback guess)

-- | Takes a list of targets. Returns the one with the highest 'guessEntropy'.
maxEntropyGuess :: [Chord] -> Chord
maxEntropyGuess targets = maximumBy (comparing (guessEntropy targets)) targets

-- | Takes a list of targets and a potential guess. Calculates all the 
-- feedbacks that could be received from the targets if that guess was made, 
-- and returns the entropy of that feedback.
guessEntropy :: [Chord] -> Chord -> Double
guessEntropy targets guess = entropy potentialFeedbacks
    where potentialFeedbacks = map (feedback guess) targets

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
    where floatDiv x y = fromIntegral x / fromIntegral y

-- | Takes a list containing elements of an ordered type. Returns a list of 
-- ints counting the number of occurences of each distinct element.
frequencies :: Ord a => [a] -> [Int]
frequencies = map length . group . sort
