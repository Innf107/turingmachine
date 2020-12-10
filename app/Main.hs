{-#LANGUAGE LambdaCase, BlockArguments#-}
module Main where

import Lib
import Data.List

data Turing = Turing {
      program::TuringFunction
    , initialState::TState
    , input::String
    , acceptedStates::[TState]
} deriving (Show, Eq)

type TState = Int

newtype TuringFunction = TuringFunction {unTF::(TState, Char) -> Maybe (TState, Char, TDirection)}

instance Show TuringFunction where
    show _ = "<TuringFunction>"

instance Eq TuringFunction where
    _ == _ = False

data TDirection = TLeft | TRight | TStay deriving (Show, Eq)

runTuring :: Turing -> ([(TState, Zipper Char)], Bool) -- Accepted
runTuring t = let res = (initialState t, Zipper [' '] (input t)) : runTuring' t (initialState t) (Zipper [] (input t)) in
        (res, fst (last res) `elem` acceptedStates t)

runTuring' :: Turing -> TState -> Zipper Char -> [(TState, Zipper Char)]
runTuring' t s mem = case unTF (program t) (s, currentT mem) of
    Just (s', c', dir) -> (s', moveDir dir (set c' mem)):runTuring' t s' (moveDir dir (set c' mem))
    Nothing -> []

moveDir :: TDirection -> Zipper Char -> Zipper Char
moveDir TLeft z = prevT z
moveDir TRight z = nextT z
moveDir TStay z = z

runTuringPrint :: Turing -> IO ()
runTuringPrint t = let (res, accepted) = runTuring t in
    mapM_ print res >> putStrLn ("\nAccepted: " <> show accepted)

t1 :: Turing
t1 = Turing {
      program=TuringFunction \case
          (0, '1') -> Just (1, '1', TRight)
          (2, '3') -> Just (3, '3', TRight)
          (1, '3') -> Just (2, '3', TRight)
          (4, '1') -> Just (1, '1', TRight)
          (3, z) | z ∈ ['6','7','8'] -> Just (4, '7', TRight)
          (4, x) | x ∉ ['1', 'B'] -> Just (5, 'B', TStay)
          _ -> Nothing
    , initialState=0
    , acceptedStates=[4]
    , input="1336"
    -- (133[678])+
    -- Output:
    -- (1337)+ # gleiche Anzahl wie input
    }

t2a :: Turing
t2a = Turing {
      program = TuringFunction \case
        (3, x) | x ∈ ['0', '1'] -> Just (0, x, TRight)
        (0, x) | x ∈ ['0', '1'] -> Just (0, x, TRight)
        (0, 'B') -> Just (1, 'B', TLeft)
        (1, x) | x ∈ ['0', '1'] -> Just (1, x, TLeft)
        (1, 'B') -> Just (2, 'B', TRight)
        _ -> Nothing
    , initialState=3
    , acceptedStates=[2]
    , input="1"
    -- Q = {q0, q1, q2, q3}
    -- E = {0, 1}
    -- Gamma = E `union` {B}
    -- F = {q2}
    }

t2b :: Turing
t2b = Turing {

    }

main :: IO ()
main = runTuringPrint t2a
