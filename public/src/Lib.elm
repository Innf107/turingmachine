module Lib exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events as E


stringList : (List String -> msg) -> List String -> Html msg
stringList setter getter = table [] [
        tbody [] [
            tr [] (List.indexedMap
                    (\i x -> input [A.value x, E.onInput (\y -> setter
                        (List.filter (not << String.isEmpty) (mapAtWithDefault "" i (const y) getter)))] [])
                    (getter ++ [""])
                )
        ]
    ]

selectList : List String -> (List String -> msg) -> List String -> Html msg
selectList options setter getter = table [] [
        tbody [] [
            tr [] (List.indexedMap
                    (\i x -> select [A.value x, E.onInput (\y -> setter
                        (List.filter (not << String.isEmpty) (mapAtWithDefault "" i (const y) getter)))]
                        (List.map (option [] << List.singleton << text) (""::options)))
                    (getter ++ [""])
                )
        ]
    ]

stringSelect : List (Attribute msg) -> List String -> Html msg
stringSelect attributes options = select attributes <| List.map (option [] << List.singleton << text) options

charInput : (Maybe Char -> msg) -> Char -> Html msg
charInput setter getter = input [A.class "char-input", A.value (String.fromChar getter), E.onInput (setter << last << String.toList)] []

const : a -> b -> a
const x _ = x

fst : (a, b) -> a
fst (x, _) = x

mapAt : Int -> (a -> a) -> List a -> List a
mapAt i f l = case (i,l) of
    (_, []) -> []
    (0, (x::xs)) -> f x :: xs
    (_, (x::xs)) -> x :: mapAt (i - 1) f xs

mapAtWithDefault : a -> Int -> (a -> a) -> List a -> List a
mapAtWithDefault default i f l = case (i,l) of
    (0, []) -> [f default]
    (_, []) -> default :: mapAtWithDefault default (i - 1) f []
    (0, (x::xs)) -> f x :: xs
    (_, (x::xs)) -> x :: mapAtWithDefault default (i - 1) f xs

hasAt : Int -> a -> List a -> Bool
hasAt i x l = case (i, l) of
    (_, []) -> False
    (0, y::ys) -> x == y
    (_, _::ys) -> hasAt (i - 1) x ys

removeAt : Int -> List a -> List a
removeAt i l = case (i, l) of
    (0, _::xs) -> xs
    (_, []) -> []
    (_, x::xs) -> x::(removeAt (i - 1) xs)

type alias Setter a s = (a -> a) -> s -> s

either : (e -> b) -> (a -> b) -> Result e a -> b
either f g x = case x of
    Err e -> f e
    Ok ok -> g ok

maybe : b -> (a -> b) -> Maybe a -> b
maybe d f x = case x of
    Nothing -> d
    Just ok -> f ok

last : List a -> Maybe a
last l = case l of
    [] -> Nothing
    (x::[]) -> Just x
    (_::xs) -> last xs
