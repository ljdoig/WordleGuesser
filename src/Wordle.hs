module Wordle
  ( Guess,
    Answer,
    GuessFeedback,
    Guesser (Guesser),
    getFeedback,
    playWordle,
    playWordleBot,
    cheatWordleBot,
    testWordleBot,
    getRandomAnswer,
  )
where

import Data.Char (toLower, toUpper)
import Data.List (all)
import Data.Maybe (isJust)
import System.Random (randomIO)

type Answer = String

type Guess = String

data LetterFeedback = Correct | WrongPosition | Wrong
  deriving (Eq, Ord)

type GuessFeedback = [LetterFeedback]

data Guesser guesserState = Guesser
  { initialGuess :: [Answer] -> [Guess] -> (Guess, guesserState),
    getNextGuess :: GetNextGuess guesserState
  }

type GetNextGuess guesserState = (Guess, guesserState) -> GuessFeedback -> (Guess, guesserState)

numAllowedGuesses = 6

wordLength = 5

failurePenalty = 4

getRandomAnswer :: [Answer] -> IO Answer
getRandomAnswer answers = do
  randInt <- randomIO :: IO Int
  return $ answers !! mod randInt (length answers)

chooseAnswer :: [Answer] -> Maybe Answer -> IO Answer
chooseAnswer answers maybeAnswer =
  case maybeAnswer of
    Just answer ->
      if answer `elem` answers
        then return answer
        else do
          putStrLn "Not a valid Wordle answer, defaulting to a random choice"
          getRandomAnswer answers
    Nothing -> getRandomAnswer answers

playWordle :: [Guess] -> [Answer] -> Maybe Answer -> IO Int
playWordle guesses answers maybeAnswer = do
  putStrLn "Beginning the game!"
  answer <- chooseAnswer answers maybeAnswer
  playWordleTurn guesses answer 1

playWordleTurn :: [Guess] -> Answer -> Int -> IO Int
playWordleTurn guesses answer n = do
  putStr $ "Enter guess #" ++ show n ++ ": "
  input <- getLine
  let guess = map toLower input
  let feedback = getFeedback guess answer
  case (guess `notElem` guesses, hasWon feedback, n == numAllowedGuesses) of
    (True, _, _) -> do
      putStrLn "Invalid guess, try again"
      playWordleTurn guesses answer n
    (_, True, _) -> do
      printColoured guess feedback
      putStrLn $ "You got it in " ++ show n ++ " guesses!"
      return n
    (_, _, True) -> do
      printColoured guess feedback
      putStrLn "You didn't get it..."
      printAnswer answer
      return $ n + failurePenalty
    _ -> do
      printColoured guess feedback
      playWordleTurn guesses answer (n + 1)

playWordleBot :: [Guess] -> [Answer] -> Guesser guesserState -> Maybe Answer -> IO Int
playWordleBot guesses answers (Guesser initialGuess getNextGuess) maybeAnswer = do
  putStrLn "Beginning the game!"
  answer <- chooseAnswer answers maybeAnswer
  let (guess, state) = initialGuess guesses answers
  playWordleBotTurn answer getNextGuess (guess, state) 1

playWordleBotTurn :: Answer -> GetNextGuess guesserState -> (Guess, guesserState) -> Int -> IO Int
playWordleBotTurn answer getNextGuess (guess, guesserState) n = do
  let feedback = getFeedback guess answer
  printColoured guess feedback
  case (hasWon feedback, n == numAllowedGuesses) of
    (True, _) -> do
      putStrLn $ "WordleGuesser got it in " ++ show n ++ " guesses!"
      return n
    (_, True) -> do
      putStrLn "WordleGuesser didn't get it..."
      printAnswer answer
      return $ n + failurePenalty
    _ ->
      let (guess', guesserState') = getNextGuess (guess, guesserState) feedback
       in playWordleBotTurn answer getNextGuess (guess', guesserState') (n + 1)

cheatWordleBot :: [Guess] -> [Answer] -> Guesser guesserState -> IO ()
cheatWordleBot guesses answers (Guesser initialGuess getNextGuess) = do
  putStrLn "Each turn, enter the word you chose and enter the feedback you get back"
  putStrLn "Your feedback must be 5 characters each either g, y, b e.g. \"gbygb\""
  putStrLn "representing the 3 colours: Green, Yellow and Black"
  putStrLn "Just hit enter to abandon"
  let (guess, state) = initialGuess guesses answers
  cheatWordleBotTurn guesses getNextGuess (guess, state) 1

getInputGuess :: Guess -> [Guess] -> IO (Maybe Guess)
getInputGuess suggestedGuess guesses = do
  putStr $ "Enter guess (suggested: " ++ suggestedGuess ++ "): "
  input <- getLine
  let guess = map toLower input
  case (input, guess `elem` guesses) of
    ("", _) -> return Nothing
    (_, True) -> return $ Just guess
    _ -> do
      putStrLn "Invalid guess, try again"
      getInputGuess suggestedGuess guesses

getInputFeedback :: IO (Maybe GuessFeedback)
getInputFeedback = do
  putStr "Feedback: "
  input <- getLine
  let valid = length input == wordLength && all (`elem` "gyb") input
  case (input, valid) of
    ("", _) -> return Nothing
    (_, True) -> return $ Just $ map letterFeedback input
    _ -> do
      putStrLn "Invalid feedback string, try again"
      getInputFeedback
  where
    letterFeedback c = case c of
      'g' -> Correct
      'y' -> WrongPosition
      'b' -> Wrong

cheatWordleBotTurn :: [Guess] -> GetNextGuess guesserState -> (Guess, guesserState) -> Int -> IO ()
cheatWordleBotTurn guesses getNextGuess (guess, guesserState) n = do
  guess <- getInputGuess guess guesses
  feedback <- if isJust guess then getInputFeedback else return Nothing
  case (guess, feedback) of
    (Just guess, Just feedback) -> do
      printColoured guess feedback
      case (hasWon feedback, n == numAllowedGuesses) of
        (True, _) -> putStrLn $ "WordleGuesser got it in " ++ show n ++ " guesses!"
        (_, True) -> putStrLn "WordleGuesser didn't get it..."
        _ ->
          let (guess', guesserState') = getNextGuess (guess, guesserState) feedback
           in cheatWordleBotTurn guesses getNextGuess (guess', guesserState') (n + 1)
    _ -> return ()

testWordleBot :: [Guess] -> [Answer] -> Guesser guesserState -> [Int]
testWordleBot guesses answers (Guesser initialGuess getNextGuess) =
  let testAnswer answer =
        testWordleBotTurn answer getNextGuess (initialGuess guesses answers) 1
   in map testAnswer answers

testWordleBotTurn :: Answer -> GetNextGuess guesserState -> (Guess, guesserState) -> Int -> Int
testWordleBotTurn answer getNextGuess (guess, guesserState) n =
  case (won, lastGuess) of
    (True, _) -> n
    (_, True) -> n + failurePenalty
    _ ->
      let (guess', guesserState') = getNextGuess (guess, guesserState) feedback
       in testWordleBotTurn answer getNextGuess (guess', guesserState') (n + 1)
  where
    feedback = getFeedback guess answer
    won = hasWon feedback
    lastGuess = n == numAllowedGuesses

printColoured :: Guess -> GuessFeedback -> IO ()
printColoured (c : cs) (f : fs) = do
  let s = [toUpper c]
  case f of
    Correct -> putStr $ "\ESC[32m " ++ s
    WrongPosition -> putStr $ "\ESC[33m " ++ s
    Wrong -> putStr $ "\ESC[31m " ++ s
  printColoured cs fs
printColoured _ _ = putStrLn "\ESC[0m"

printAnswer :: Answer -> IO ()
printAnswer answer = putStrLn $ "The answer was:\n" ++ concatMap (\c -> [' ', toUpper c]) answer

hasWon :: GuessFeedback -> Bool
hasWon = all (== Correct)

getFeedback :: Guess -> Answer -> GuessFeedback
getFeedback guess answer =
  let (guess', answer', areCorrect) = filterCorrect guess answer
      areWrongPosition = getWrongPosition guess' answer'
   in knitFeedbacks areCorrect areWrongPosition

knitFeedbacks :: [Bool] -> [Bool] -> [LetterFeedback]
knitFeedbacks (c : cs) (wp : wps) =
  case (c, wp) of
    (True, _) -> Correct : knitFeedbacks cs (wp : wps)
    (_, True) -> WrongPosition : knitFeedbacks cs wps
    _ -> Wrong : knitFeedbacks cs wps
knitFeedbacks cs _ = map (\c -> if c then Correct else Wrong) cs

filterCorrect :: String -> String -> (String, String, [Bool])
filterCorrect (g : gs) (a : as)
  | g == a = (gs', as', True : correct')
  | otherwise = (g : gs', a : as', False : correct')
  where
    (gs', as', correct') = filterCorrect gs as
filterCorrect _ _ = ("", "", [])

getWrongPosition :: String -> String -> [Bool]
getWrongPosition (g : gs) as =
  let (as', isPresent) = removeIfPresent g as
      areWrongPosition' = getWrongPosition gs as'
   in (isPresent : areWrongPosition')
getWrongPosition _ _ = []

removeIfPresent :: Char -> String -> (String, Bool)
removeIfPresent c (x : xs)
  | c == x = (xs, True)
  | otherwise =
    let (xs', isPresent) = removeIfPresent c xs
     in (x : xs', isPresent)
removeIfPresent _ _ = ([], False)
