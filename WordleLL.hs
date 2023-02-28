import Data.Char
import Graphics.Gloss
import System.Random

data State = State [String] Int

data Result
  = EndOfGame Double State
  | ContinueGame State

type Game = Action -> Action -> State -> Result

data Action = Action [Char] -- a move for a player is a String
  deriving (Eq)

wordle :: Game
wordle move answer (State history attempts)
  | move == answer = EndOfGame 1 wordle_start
  | attempts == 1 = EndOfGame 0 wordle_start
  | otherwise =
      do
        let response = filter_word move answer
        let Action guess = move
        ContinueGame (State (history ++ [show move ++ "|" ++ response]) (attempts - 1))

wordle_start = State [] 6

-- show and read actions just as the String
instance Show Action where
  show (Action i) = show i

type WordleState = (Int, Int) -- wins, losses

start :: Game -> State -> WordleState -> IO WordleState
start game start_state ws =
  let (wins, losses) = ws
   in do
        putStrLn ("Stats: " ++ show wins ++ " wins " ++ show losses ++ " losses")
        putStrLn ("Would you like to exit?(y/n)") -- ask to leave the game
        line <- getLine
        if line == "y"
          then do
            putStrLn "Thanks for playing!"
            return ws
          else do
            putStrLn ("Would you like to play with a GUI (y/n)?") -- ask to play with or without GUI
            line <- getLine
            if line == "y"
              then do
                answer <- randomWord
                playGameGUI game (Action answer) (ContinueGame start_state) ws
              else
                if line == "n"
                  then do
                    answer <- randomWord
                    playGame game (Action answer) (ContinueGame start_state) ws
                  else do
                    putStrLn "Invalid input"
                    start game start_state ws

-- with GUI
playGameGUI :: Game -> Action -> Result -> WordleState -> IO WordleState
playGameGUI game answer (ContinueGame state) ws =
  do
    putStrLn ("--------------------------")
    displayInfo state

    putStrLn ("Enter your guess")
    line <- getLine

    if length line == 5
      then
        if (all isLower line)
          then do
            playGameGUI game answer (game (Action line) answer state) ws
          else do
            putStrLn ("Illegal move " ++ line ++ ". Please input all lowercase.")
            playGameGUI game answer (ContinueGame state) ws
      else do
        putStrLn ("Illegal move " ++ line ++ ". Please input a 5 letter guess.")
        playGameGUI game answer (ContinueGame state) ws
playGameGUI game (Action answer) (EndOfGame val start_state) ws =
  do
    newws <- update_wordle_state val (Action answer) ws
    if (val == 1)
      then do displayAnswer "Congratulations :)" answer
      else do displayAnswer "Nice try :)" answer
    start game start_state newws

-- without GUI
playGame :: Game -> Action -> Result -> WordleState -> IO WordleState
playGame game answer (ContinueGame state) ws =
  do
    putStrLn ("--------------------------")
    displayInfo state

    putStrLn ("Enter your guess")
    line <- getLine

    if length line == 5
      then
        if (all isLower line)
          then do
            playGame game answer (game (Action line) answer state) ws
          else do
            putStrLn ("Illegal move " ++ line ++ ". Please input all lowercase.")
            playGame game answer (ContinueGame state) ws
      else do
        putStrLn ("Illegal move " ++ line ++ ". Please input a 5 letter guess.")
        playGame game answer (ContinueGame state) ws
playGame game (Action answer) (EndOfGame val start_state) ws =
  do
    newws <- update_wordle_state val (Action answer) ws
    start game start_state newws

randomWord = (word_bank !!) <$> randomRIO (0, length word_bank - 1)

filter_word (Action guess) (Action answer) = filter_guess guess (filter_answer guess answer) 0

filter_answer :: String -> String -> String
filter_answer [] answer = []
filter_answer (x : xs) (h : t)
  | x == h = 'G' : (filter_answer xs t)
  | otherwise = h : (filter_answer xs t)

filter_guess [] answer n = []
filter_guess (x : xs) answer n
  | answer !! n == 'G' = 'G' : (filter_guess xs answer (n + 1))
  | x `elem` answer = 'Y' : (filter_guess xs answer (n + 1))
  | otherwise = 'X' : (filter_guess xs answer (n + 1))

displayInfo (State history attempts) =
  do
    putStrLn ("You have " ++ show attempts ++ " attempts left.")
    mapM_ putStrLn history

update_wordle_state val (Action answer) (wins, losses)
  | val == 1 = do
      putStrLn "You Won"
      return (wins + 1, losses)
  | otherwise = do
      putStrLn ("You lost!")
      putStrLn ("The correct answer: " ++ answer)
      return (wins, losses + 1)

begin = start wordle wordle_start (0, 0)

word_bank = ["which", "their", "would", "there", "could", "other", "about", "great", "these", "after", "first", "never", "where", "those", "shall", "being", "might", "every", "think", "under", "found", "still", "while", "again", "place", "young", "years", "three", "right", "house", "whole", "world", "thing", "night", "going", "heard", "heart", "among", "asked", "small", "woman", "whose", "quite", "words", "given", "taken", "hands", "until", "since", "light", "began", "large", "water", "works", "often", "stood", "power", "money", "order", "means", "round", "voice", "white", "point", "state", "above", "death", "least", "known", "along", "leave", "alone", "women", "times", "speak", "forth", "terms", "cried", "child", "human", "short", "cause", "seems", "bring", "doubt", "black", "sense", "close", "truth", "ought", "party", "ready", "force", "early", "earth", "sight", "spoke", "story", "later", "added", "stand", "nicht", "again", "miles", "comes", "table", "hours", "river", "happy", "clear", "sound", "makes", "blood", "comme", "doing", "avait", "tried", "front", "quill", "peace", "lived", "horse", "wrote", "paper", "cette", "chief", "books", "visit", "heavy", "knows", "loved", "carry", "plain", "sweet", "write", "trees", "below", "wrong", "reach", "noble", "parts", "agree", "moved", "enemy", "worth", "green", "third", "mouth", "sleep", "fresh", "faith", "there", "smile", "usual", "bound", "quiet", "etext", "court", "youth", "piece", "meant", "world", "seven", "tears", "value", "broke", "fight", "stone", "begin", "learn", "lines", "grand", "takes", "month", "girls", "gives", "eight", "scene", "lives", "drawn", "fifty", "field", "chair", "named", "allow", "music", "fixed", "study", "spent", "trust", "break", "equal", "threw", "watch", "looks", "built", "using", "spite", "moral", "walls", "touch", "steps", "offer", "house", "dress", "lying", "grave", "legal", "lower", "other", "cases", "night", "shown", "names", "board", "faire", "glass", "share", "forms", "class", "start", "shook", "train", "enter", "prove", "floor", "worse", "sorry", "pride", "place", "marry", "crowd", "shore", "drink", "judge"]

displayAnswer message answer =
  display
    ( InWindow
        "WORDLE RESULT" -- window title
        (1400, 150) -- window size
        (10, 10) -- window position
    )
    white -- background color
    (pic message answer) -- picture to display

pic message answer =
  Translate (-170) (-20) $ -- shift the text to the middle of the window
    Scale 0.3 0.3 $ -- display it half the original size
      Text (message ++ "! The answer is: " ++ answer) -- text to display
