{-# LANGUAGE BlockArguments #-}



data State = State InternalState [Action] -- internal_state available_actions
         deriving (Ord, Eq, Show)

data Result = EndOfGame Double State    -- end of game: value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> Action

-- Wordle 

type Action = String

type InternalState = ([String], Int)



type TournammentState = (Int, Int)





checkWin userWordSoFar answer =
    userWordSoFar == answer

compareWord [] [] result answer = result
compareWord (firstCharOfUserWord : restOfUserWord) (firstCharOfAnswer : restOfAnswer) result answer
  | firstCharOfUserWord == firstCharOfAnswer = compareWord restOfUserWord restOfAnswer (result++['G']) answer
  | firstCharOfUserWord `elem` answer = compareWord restOfUserWord restOfAnswer (result++['Y']) answer
  | otherwise = compareWord restOfUserWord restOfAnswer (result++['X']) answer


-- given a move and a state, return a result 
-- result is either endofgame or continue game 
wordle :: Game
wordle move (State (guessesSoFar, numLives) possibleActions) = 
  if numLives == 1
    then if compareWord move "happy" [] "happy" == "GGGGG"
        then EndOfGame 1 (State ([], 0) possibleActions)
        else EndOfGame (-1) (State ([], 5) possibleActions)
    else
        do
        let response = compareWord move "happy" [] "happy"
        ContinueGame (State (guessesSoFar++[move ++ " | " ++ response], numLives-1) possibleActions)

--   | compareWord move "happy" [] "happy" == "GGGGG" = EndOfGame 1 (State ([], 0) possibleActions)
--   | numLives == 0 = EndOfGame (-1) (State ([], 5) possibleActions)
--   | otherwise = 
--     do
--     let response = compareWord move "happy" [] "happy"
--     ContinueGame (State (guessesSoFar++[move ++ " | " ++ response], numLives-1) possibleActions)


play :: Game -> State -> TournammentState -> IO TournammentState
play game currState tournamentState =
    let (wins, losses) = tournamentState in
    do
        putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses ")
        personPlay game (ContinueGame currState) tournamentState




personPlay :: Game -> Result -> TournammentState -> IO TournammentState
personPlay game (EndOfGame val startStateForNewGame) tournamentState =
    do
        updatedTournamentState <- updateTournamentState val tournamentState
        play game startStateForNewGame updatedTournamentState

personPlay game (ContinueGame currState) tournamentState =
    do
        let State internalState possibleActions = currState
        putStrLn "======================="
        displayExtraInfoAboutInternalState internalState
        putStrLn ("Do an action: "++ show possibleActions)
        action <- getLine
        personPlay game (game action currState) tournamentState

displayExtraInfoAboutInternalState internalState=
    do
        let (guessesSoFar, numLives) = internalState
        putStrLn ("You have " ++ show numLives ++ " lives left.")
        mapM_ putStrLn guessesSoFar


updateTournamentState val (wins,losses)
  | val > 0 = do
      putStrLn "You Won"
      return (wins+1,losses)
  | otherwise = do
      putStrLn "You lost"
      return (wins,losses+1)


