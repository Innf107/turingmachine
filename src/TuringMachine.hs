module TuringMachine where

import Lib
import Data.List

data Turing = Turing {
      program::TuringFunction
    , initialState::TState
    , acceptedStates::[TState]
} deriving (Show, Eq)

type TState = Int

newtype TuringFunction = TuringFunction {unTF::(TState, Char) -> Maybe (TState, Char, TDirection)}

instance Show TuringFunction where
    show _ = "<TuringFunction>"

instance Eq TuringFunction where
    _ == _ = False

data TDirection = TLeft | TRight | TStay deriving (Show, Eq)

runTuring :: Turing -> String -> ([(TState, Zipper Char)], Bool) -- Accepted
runTuring t input = let res = (initialState t, Zipper [' '] input) : runTuring' t (initialState t) (Zipper [] input) in
        (res, fst (last res) `elem` acceptedStates t)

runTuring' :: Turing -> TState -> Zipper Char -> [(TState, Zipper Char)]
runTuring' t s mem = case unTF (program t) (s, currentT mem) of
    Just (s', c', dir) -> (s', moveDir dir (set c' mem)):runTuring' t s' (moveDir dir (set c' mem))
    Nothing -> []

moveDir :: TDirection -> Zipper Char -> Zipper Char
moveDir TLeft z = prevT z
moveDir TRight z = nextT z
moveDir TStay z = z

runTuringPrint :: Turing -> String -> IO ()
runTuringPrint t input = let (res, accepted) = runTuring t input in
    mapM_ print res >> putStrLn ("\nAccepted: " <> show accepted)
