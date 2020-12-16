{-# LANGUAGE FlexibleInstances #-}

module Lib where

import Data.Maybe
import GHC.Show

data Zipper a = Zipper [a] [a] deriving (Eq)

instance Show (Zipper Char) where
    show (Zipper ls (r:rs)) = reverse ls ++ "[" ++ [r] ++ "]" ++ rs
    show (Zipper ls []) = ls ++ "[]"

next :: Zipper a -> Zipper a
next (Zipper _ []) = error "next: empty Zipper"
next (Zipper ls (r:rs)) = Zipper (r:ls) rs

next' :: a -> Zipper a -> Zipper a
next' d (Zipper ls [r]) = Zipper (r:ls) [d]
next' d (Zipper ls []) = Zipper (d:ls) []
next' _ z = next z

prev :: Zipper a -> Zipper a
prev (Zipper [] _) = error "prev: empty Zipper"
prev (Zipper (l:ls) rs) = Zipper ls (l:rs)

prev' :: a -> Zipper a -> Zipper a
prev' d (Zipper [l] rs) = Zipper [d] (l:rs)
prev' d (Zipper [] rs) = Zipper [] (d:rs)
prev' _ z = prev z

current :: Zipper a -> Maybe a
current (Zipper _ (x:_)) = Just x
current _ = Nothing

current' :: a -> Zipper a -> a
current' d = fromMaybe d . current

set :: a -> Zipper a -> Zipper a
set x (Zipper ls (r:rs)) = Zipper ls (x:rs)
set x (Zipper ls []) = Zipper ls [x]


nextT :: Zipper Char -> Zipper Char
nextT = next' '_'

prevT :: Zipper Char -> Zipper Char
prevT = prev' '_'

currentT :: Zipper Char -> Char
currentT = current' '_'

(∈) :: (Foldable f, Eq a ) => a -> f a -> Bool
(∈) = elem

(∉) :: (Foldable f, Eq a ) => a -> f a -> Bool
(∉) = notElem

