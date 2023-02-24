{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use print" #-}
import Data.List
import Prelude hiding (Nothing, Just, Maybe)
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant bracket" #-}

compareWord [] [] result answer = result
compareWord (firstCharOfUserWord : restOfUserWord) (firstCharOfAnswer : restOfAnswer) result answer
  | (firstCharOfUserWord == firstCharOfAnswer) = compareWord restOfUserWord restOfAnswer (result++['G']) answer
  | (firstCharOfUserWord `elem` answer) = compareWord restOfUserWord restOfAnswer (result++['Y']) answer
  | otherwise = compareWord restOfUserWord restOfAnswer (result++['X']) answer

-- compareWord "hello" "riper" [] "riper"
-- expect "XYXXX"

-- compareWord "whole" "riper" [] "riper"
-- expect "XXXXY"

-- compareWord "heals" "riper" [] "riper"
-- expect "XYXXX"

-- compareWord "tired" "riper" [] "riper"
-- expect "XGYGX"

-- compareWord "niter" "riper" [] "riper"
-- expect "XGXGG"

-- compareWord "jiber" "riper" [] "riper"
-- expect "XGXGG"

-- compareWord "riper" "riper" [] "riper"
-- expect "GGGGG"
type GameState = String

-- play :: (Eq t, Num t) => t -> IO()
play numberOfTries = 
    if numberOfTries == 0
        then 
            putStrLn("Bop! You lost! Gg <3")
    else 
        do 
            guess <- getLine
            if ((compareWord guess "happy" [] "happy") == "GGGGG")
                then putStrLn("You won! Well done :)")
            else 
                do 
                    let gameState = (compareWord guess "happy" [] "happy") 
                    putStrLn(show gameState)
                    play (numberOfTries-1)




