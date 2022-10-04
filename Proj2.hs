{-
  Author  : Lachlan Doig
  ID      : 1064152
  Purpose : Implement guessing and answering for a logical guessing game

  The game of Musician is played between a composer and performer. The
  composer selects a chord made up of 3 distinct pitches, where each pitch has
  a note (either A, B, C, D, E, F or G) and an octave (either 1, 2 or 3). There
  are therefore 21 possible pitches (7 x 3) and 1330 possible chords
  (21 choose 3). The composer's chosen chord is the target, and the performer's
  job is to find all pitches in the target in as few guesses as possible. For
  each guess, the performer selects 3 distinct pitches and the composer gives
  feedback about how close they are to the target, in the form of 3 integers:
      1) the number of correct pitches (correct in both note and octave)
      2) the number of pitches with the correct note but an incorrect octave
      3) the number of pitches with the correct octave but an incorrect note
  For 2) and 3), repeated notes or octave in the guess will only be considered
  correct if they are also repeated in the target. E.g. if you guess A1, B1, C1
  when the target is D1, E1, F2, then the feedback will be (0, 0, 2), because
  the octave '1' appears only twice in the target. Pitches counted in 1) are not
  counted in either 2) or 3). Some more examples:

  Target      Guess       Feedback
  A1,B2,C3    A1,B2,C1    2,1,0
  A1,B1,C1    A1,D1,E1    1,0,2
  A1,B1,C1    D1,E2,F3    0,0,1
  A1,A2,B3    A1,B1,B2    1,1,1

  This program implements the composer's answering logic to provide this
  feedback but also the performer's guessing logic to deduce the target in a
  small number of guesses. This approach first generates all 1330 possible
  targets. Each guess, we trim down the set of remaining targets to the ones
  consistent with all of the feedback given so far, and we track these remaining
  targets as the current game state. From them, we choose the next guess using
  Shannon's entropy as a scoring metric to select the guess that holds the most
  information about the target. Specifically, a potential guess is evaluated by
  calculating all the feedbacks that could be received from the remaining
  targets. We then calulate the entropy of these feedbacks, to provide a score
  for this potential guess. Higher entropy is desirable as it means the
  feedbacks are more evenly distributed, so the true feedback we receive
  contains high information and is likely to cut down the remaining targets as
  much as possible. .

  For the very first guess, we calculated and hardcoded the guess that has the
  highest entropy to save computing time; from then we repeatedly filter the
  remaining targets and select the highest entropy guess until we find the
  target.
-}

module Proj2
  ( Pitch,
    toPitch,
    feedback,
    GameState,
    initialGuess,
    nextGuess,
    Chord,
    allChords,
  )
where

import Data.List (delete, group, maximumBy, sort, subsequences)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Text.Read (readMaybe)

-- | A note type, enumerating all possible notes in a pitch.
data Note = A | B | C | D | E | F | G
  deriving (Eq, Read, Show, Enum, Bounded)
  
-- | An octave type, enumerating all possible octaves in a pitch.
data Octave = O1 | O2 | O3
  deriving (Eq, Read, Show, Enum, Bounded)

-- | A pitch type, made up of both a note and an octave. Shown as a simple
-- two-character string e.g. show (Pitch A O1) -> "A1".
data Pitch = Pitch Note Octave
  deriving Eq
instance Show Pitch where show (Pitch note oct) = show note ++ tail (show oct)

-- | A chord type-synonym. Chords are represented as a list of 3 pitches.
type Chord = [Pitch]

-- | A game-state type, representing the list of chords that are still possible
-- to guess at a given point in the game, based on all the feedback for guesses 
-- so far. Initially, this will just be every possible chord.
newtype GameState = GameState [Chord]

-- | A feedback type-synonym. Feedback is given as a tuple of 3 integers, 
-- according to the rules described above.
type Feedback = (Int, Int, Int)

-- | List containing all possible pitches, the cartesian product of the set of
-- all notes and set of all octaves. Used to generate all possible chords.
allPitches :: [Pitch]
allPitches = [Pitch note octave | note <- [minBound..], octave <- [minBound..]]

-- | List containing all possible chords, each of which is a list of pitches of
-- length 3 with no repeated pitches. Starting point for possible guesses.
allChords :: [Chord]
allChords = filter ((== 3) . length) (subsequences allPitches)

-- | Parses a string describing a pitch, returning the corresponding 
-- 'Just Pitch' for a 2-character string representing a valid pitch, and Nothing 
-- otherwise. E.g. toPitch "A3" -> Just (Pitch A O3) ; toPitch "A4" -> Nothing
toPitch :: String -> Maybe Pitch
toPitch [noteChar, octaveChar] =
  case (readMaybe [noteChar], readMaybe ['O', octaveChar]) of
    (Just note, Just octave) -> Just (Pitch note octave)
    _ -> Nothing
toPitch _ = Nothing
    
-- | Takes two chords, a guess and a target, and returns feedback according
-- to the rules described above. This function is commutative, so 
-- 'feedback guess target' and 'feedback target guess' are identical.
feedback :: Chord -> Chord -> Feedback
feedback guess target =
  (numSamePitch, numSameNote - numSamePitch, numSameOctave - numSamePitch)
  where numSamePitch = numCommonElements target guess
        (targetNotes, targetOctaves) = splitChord target
        (guessNotes, guessOctaves) = splitChord guess
        numSameNote = numCommonElements targetNotes guessNotes
        numSameOctave = numCommonElements targetOctaves guessOctaves

-- | Takes two lists whose elements can be compared for equality, and returns
-- the number of elements that are common to both lists. Repeated elements are 
-- only counted if they are repeated in both lists, so 
-- numCommonElements [1,1,2] [1,2,2] -> 2.
numCommonElements :: Eq a => [a] -> [a] -> Int
numCommonElements [] _ = 0
numCommonElements (x:xs) ys
  | x `elem` ys = 1 + numCommonElements xs (delete x ys)
  | otherwise = numCommonElements xs ys

-- | Takes a chord and splits it into a tuple containing a list of 3 notes and a
-- list of 3 octaves.
splitChord :: Chord -> ([Note], [Octave])
splitChord [Pitch n1 o1, Pitch n2 o2, Pitch n3 o3] = ([n1,n2,n3], [o1,o2,o3])
splitChord _ = error "invalid chord, should have exactly 3 pitches"

-- | A tuple containing the first chord guessed and the list of remaining 
-- targets to guess in future. The guess was initially calculated using 
-- 'maxEntropyGuess allChords', but was then hardcoded to save time, as the 
-- result will never change.
initialGuess :: (Chord, GameState)
initialGuess = (guess, GameState (delete guess allChords))
  where guess = map (fromJust . toPitch) ["E2","F3","G3"]

-- | Takes in the guessed chord and remaining targets from the previous turn,
-- as well as the feedback given from this guess. The targets are filtered down
-- to those consistent with the feedback, and the nextGuess is selected from
-- the updated targets using maxEntropyGuess.
nextGuess :: (Chord, GameState) -> Feedback -> (Chord, GameState)
nextGuess (oldGuess, GameState oldTargets) yourFeedback =
  (newGuess, GameState (delete newGuess newTargets))
  where newTargets = consistentTargets yourFeedback oldGuess oldTargets
        newGuess = maxEntropyGuess newTargets

-- | Takes a feedback, the guess that generated the feedback and a list of 
-- targets. Returns a filtered list of targets, containing only those consistent 
-- with the feedback, i.e. for the given guess they generate the same feedback.
consistentTargets :: Feedback -> Chord -> [Chord] -> [Chord]
consistentTargets yourFeedback guess =
  filter ((== yourFeedback) . feedback guess)

-- | Takes a list of targets and returns the one with the highest 'guessEntropy'
-- with respect to the targets.
maxEntropyGuess :: [Chord] -> Chord
maxEntropyGuess targets = maximumBy (comparing (guessEntropy targets)) targets

-- | Takes a list of targets and a potential guess. Calculates all the feedbacks 
-- that could be received from the targets if that guess was made, and returns 
-- the entropy of that feedback.
guessEntropy :: [Chord] -> Chord -> Double
guessEntropy targets guess = entropy potentialFeedbacks
  where potentialFeedbacks = map (feedback guess) targets

-- | Takes a list containing elements of an ordered type. Returns Shannon's
-- entropy of the list in bits.
entropy :: Ord a => [a] -> Double
entropy xs = sum [- p * logBase 2 p | p <- elementDistribution xs]

-- | Takes a list containing elements of an ordered type. For each distinct 
-- element, calculates what proportion of the list contains that element. 
-- E.g. elementDistribution "aaba" -> [0.75,0.25], as 3/4 of the list is 'a' and 
-- 1/4 of the list is 'b'.
elementDistribution :: Ord a => [a] -> [Double]
elementDistribution xs = [freq `floatDiv` length xs | freq <- frequencies xs]
  where floatDiv x y = fromIntegral x / fromIntegral y

-- | Takes a list containing elements of an ordered type. For each distinct 
-- element of a list, counts how many times the element occurs. 
-- E.g. frequencies "aaba" -> [3,1], as 'a' occurs 3 times and 'b' occurs once.
frequencies :: Ord a => [a] -> [Int]
frequencies = map length . group . sort
