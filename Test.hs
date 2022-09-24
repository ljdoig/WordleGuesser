module Test where

import Data.Maybe (fromJust)
import Main
import Proj2

testFeedback :: Bool
testFeedback = answers == zipWith feedback (parse targets) (parse guesses)
  where
    parse = map toChord
    targets =
      [ "A1 B2 A3",
        "A1 B2 C3",
        "A1 B1 C1",
        "A3 B2 C1"
      ]
    guesses =
      [ "A1 A2 B1",
        "A1 A2 A3",
        "A2 D1 E1",
        "C3 A2 B1"
      ]
    answers = [(1, 2, 1), (1, 0, 2), (0, 1, 2), (0, 3, 3)]

loopQuick :: [Pitch] -> [Pitch] -> GameState -> Int -> IO Int
loopQuick target guess other guesses = do
  let answer = feedback target guess
  if answer == (3, 0, 0)
    then do
      putStrLn $ show guesses ++ " guesses"
      return guesses
    else do
      let (guess', other') = nextGuess (guess, other) answer
      loopQuick target guess' other' (guesses + 1)

guessQuick :: Chord -> IO Int
guessQuick target = do
  let (guess, other) = initialGuess
  loopQuick target guess other 1

guessTestsQuick :: [Chord] -> IO [Int]
guessTestsQuick [] = return []
guessTestsQuick (x : xs) =
  do
    guess <- guessQuick x
    rest <- guessTestsQuick xs
    return (guess : rest)

run :: IO ()
run = do
  guesses <- guessTestsQuick targets
  print guesses
  putStrLn ("Average: " ++ show (fromIntegral (sum guesses) / fromIntegral (length guesses)))

guessTests :: [Chord] -> IO ()
guessTests = foldr ((>>) . guessTest) (return ())

runLong :: IO ()
runLong = guessTests targets

targets :: [Chord]
targets = chords
