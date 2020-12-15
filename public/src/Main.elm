module Main exposing (main)

import Browser

import Html exposing (..)
import Html.Attributes as A
import Html.Events as E

import Lib exposing (..)

main = Browser.document {init=init, update=update, subscriptions=subs, view=view}

type alias State = String

type TFMovement = TLeft | TRight | TStay

movementToString : TFMovement -> String
movementToString x = case x of
    TLeft -> "Links"
    TRight -> "Rechts"
    TStay -> "Bleib"

type alias TFInputState = State -- might add variables like X later

type alias TFOutputState = State -- might add variables like X later

type alias TuringFunction = {
          inputState : TFInputState
        , inputChar : Char
        , outputState : TFOutputState
        , outputChar : Char
        , outputMovement : TFMovement
    }

type alias Model = {
          states : List State
        , initialState : State
        , acceptedStates : List State
        , delta : List TuringFunction
    }

init : () -> (Model, Cmd Msg)
init _ = ({
      states = ["q0"]
    , initialState = "q0"
    , acceptedStates = []
    , delta = []
    }, Cmd.none)

subs : Model -> Sub Msg
subs _ = Sub.none

type Msg = NOP
         | UpdateStates (List String)
         | UpdateAcceptedStates (List String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
        NOP -> (model, Cmd.none)
        UpdateStates newstates -> ({model|states=newstates}, Cmd.none)
        UpdateAcceptedStates newstates -> ({model|acceptedStates=newstates}, Cmd.none)

view : Model -> Browser.Document Msg
view model = {
          title="Turingmaschinen Simulator"
        , body=
            navbar
         ++ explanation
         ++ turingInput model
    }

navbar : List (Html Msg)
navbar = [
        nav [A.class "navbar"] [
              h3 [] [text "Turingmaschinen Simulator"]
            , h5 [] [text "Source Code und weitere Tools"]
        ]
    ]

explanation : List (Html Msg)
explanation = [
          p [] [text "Eine (deterministische) Turingmaschine ist definiert als der 6-Tupel ", b [] [text "M"],
            text " = (Q, Σ, Γ, δ, q", sub [] [text "0"] , text ", F). (",
            a [A.href "https://de.wikipedia.org/wiki/Turingmaschine"] [text "Weitere informationen"], text ")"
        ]
        , p [] [text "Dabei ist:"]
        , ul [] [
              li [] [b [] [text "Q"], text " die endliche und nicht leere Menge aller Zustände von M"]
            , li [] [b [] [text "Σ"], text " das Eingabealphabet, also die (endliche) Menge aller akzeptierten Zeichen für die Eingabe"]
            , li [] [b [] [text "Γ"], text " das Bandalphabet, also die (endliche) Menge aller Zeichen die auf dem Band stehen können.",
                    text "Es gilt immer Γ ⊇ Σ ∪ {_}, wobei _ das 'Blank' Zeichen, also das Zeichen einer leeren Speicherzelle auf dem Band der Turingmaschine ist."]
            , li [] [b [] [text "δ"], text " die (partielle) Überführungsfunktion der Turingmaschine. δ : Q × Γ → Q × Γ × {links, bleib, rechts}"]
            , li [] [b [] [text "q", sub [] [text "0"]], text " der Startzustand der Turingmaschine. q", sub [] [text "0"], text " "]
            , li [] [b [] [text "F"], text " die Menge der zu akzeptierenden Endzustände. F ⊆ Q"]
        ]
        , p [] [text "Dieser Simulator benötigt nur die Eingaben für Q, δ, q", sub [] [text "0"], text " und F."]
    ]

turingInput : Model -> List (Html Msg)
turingInput model = [
          h4 [] [text "Q"]
        , stringList UpdateStates (model.states)

        , h4 [] [text "q", sub [] [text "0"]]
        , select [] (List.map (option [] << List.singleton << text) model.states)

        , h4 [] [text "F"]
        , selectList model.states UpdateAcceptedStates model.acceptedStates

        , div [] <| List.map (p [] << List.singleton << text) model.acceptedStates

        , h4 [] [text "δ"]
        {-, table [] [
            tbody [] <| List.map (\tf -> tr [] [
                  td [] [text "(", stringSelect [A.value tf.inputState, E.onInput (Debug.todo "SetInputState")] model.states,
                    text ", ", charInput (Debug.todo "SetInputChar") tf.inputChar]
                , td [] [text "→"]
                , td [] [text "(", stringSelect [A.value tf.outputState, E.onInput (Debug.todo "SetOutputState")] model.states,
                    text ", ", charInput (Debug.todo "SetOutputChar") tf.outputChar,
                    text ", ", stringSelect [A.value (movementToString tf.outputMovement), E.onInput (Debug.todo "SetOutputMovement")] ["Links", "Rechts", "Bleib"]]
                , td [] [button [E.onClick (Debug.todo "RemoveTF")] [text "-"]]
                ]
            ) model.delta ++ [tr [] [button [E.onClick (Debug.todo "AddTF")] [text "+"]]]
        ]-}
    ]

