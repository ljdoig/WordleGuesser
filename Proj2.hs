module Proj2
  ( Pitch,
    toPitch,
    feedback,
    GameState,
    initialGuess,
    nextGuess,
  )
where

import Data.Char (digitToInt)
import Data.List (delete, sort)
import Data.Maybe (fromJust)

data Pitch = Pitch Char Int
  deriving (Eq, Ord)

instance Show Pitch where
  show (Pitch note oct) = [note, head (show oct)]

newtype GameState = GameState [[Pitch]]

notes = ['A' .. 'G']

octs = [1 .. 3]

pitches = [Pitch n o | n <- notes, o <- octs]

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

feedback :: [Pitch] -> [Pitch] -> (Int, Int, Int)
feedback target guess = (samePitch, sameNote - samePitch, sameOct - samePitch)
  where
    samePitch = countCommon target guess
    (targetNotes, targetOcts) = splitPitches target
    (guessNotes, guessOcts) = splitPitches guess
    sameNote = countCommon targetNotes guessNotes
    sameOct = countCommon targetOcts guessOcts

countCommon :: Eq a => [a] -> [a] -> Int
countCommon (x : xs) ys
  | x `elem` ys = 1 + countCommon xs (delete x ys)
  | otherwise = countCommon xs ys
countCommon [] _ = 0

splitPitches :: [Pitch] -> ([Char], [Int])
splitPitches [] = ([], [])
splitPitches (Pitch note oct : tail) = (note : notes, oct : octs)
  where
    (notes, octs) = splitPitches tail

initialGuess :: ([Pitch], GameState)
initialGuess = (head chords, GameState (tail chords))

nextGuess :: ([Pitch], GameState) -> (Int, Int, Int) -> ([Pitch], GameState)
nextGuess (_, GameState (x : xs)) _ = (x, GameState xs)
nextGuess _ _ = error "no guesses left"
