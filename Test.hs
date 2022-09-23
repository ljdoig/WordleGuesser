import Data.Maybe (fromJust)
import Proj2

toChord :: String -> [Pitch]
toChord = fromJust . mapM toPitch . words

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
