{-#LANGUAGE LambdaCase, BlockArguments, OverloadedStrings#-}
module Main where

import Web.Scotty as S

import TuringMachine
import Lib ((∈), (∉))

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
    -- Q = {q0, q1, q2, q3}
    -- E = {0, 1}
    -- Gamma = {0, 1, B}
    -- F = {q2}
    }

t2b :: Turing
t2b = Turing {
    program = TuringFunction \case
        (0, x) | x ∈ ['0', '1'] -> Just (0, x, TRight)
        (0, 'B') -> Just (1, 'B', TLeft)
        (1, '0') -> Just (2, '0', TLeft)
        --(1, '1') -> Just (5, '1', TStay)
        (2, '0') -> Just (3, '0', TLeft)
        (2, '1') -> Just (5, '1', TStay)
        (3, '0') -> Just (4, '0', TStay)
        (3, '1') -> Just (5, '1', TStay)
        _ -> Nothing
    , initialState=0
    , acceptedStates=[4, 3, 2]
    }

t2c :: Turing
t2c = Turing {
    program = TuringFunction \case
        (0, x) | x ∈ ['0', '1'] -> Just (1, x, TRight)
        (1, x) | x ∈ ['0', '1'] -> Just (1, x, TRight)
        (1, 'B') -> Just (2, '1', TRight)
        (2, 'B') -> Just (3, '1', TStay)
        _ -> Nothing
    , initialState=0
    , acceptedStates=[3]
    }

main :: IO ()
main = scotty 4567 $ do
    get "/" $ file "public/index.html"
    get "/index.js" $ file "public/index.js"

