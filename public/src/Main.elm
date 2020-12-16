module Main exposing (main)

import Browser

import Html exposing (..)
import Html.Attributes as A
import Html.Events as E

import Http as H

import Json.Decode as JD

import Json.Encode as JE

import Lib exposing (..)

main = Browser.document {init=init, update=update, subscriptions=subs, view=view}

type alias State = String

type TFMovement = TLeft | TRight | TStay

movementToString : TFMovement -> String
movementToString x = case x of
    TLeft -> "Links"
    TRight -> "Rechts"
    TStay -> "Bleib"

movementFromString : String -> TFMovement
movementFromString x = case x of
    "Links" -> TLeft
    "Rechts" -> TRight
    _ -> TStay

type alias TFInputState = State -- might add variables like X later

type alias TFOutputState = State -- might add variables like X later

type alias TuringFunction = {
          inputState : TFInputState
        , inputChar : Char
        , outputState : TFOutputState
        , outputChar : Char
        , outputMovement : TFMovement
    }

emptyTF : Model -> TuringFunction
emptyTF model = {  inputState = Maybe.withDefault "" (List.head model.states)
                 , inputChar = '_'
                 , outputState = Maybe.withDefault "" (List.head model.states)
                 , outputChar = '_'
                 , outputMovement = TLeft
                 }

modifyInputState : Setter TFInputState TuringFunction
modifyInputState f s = {s|inputState = f s.inputState}

modifyInputChar : Setter Char TuringFunction
modifyInputChar f s = {s|inputChar = f s.inputChar}

modifyOutputState : Setter TFOutputState TuringFunction
modifyOutputState f s = {s|outputState = f s.outputState}

modifyOutputChar : Setter Char TuringFunction
modifyOutputChar f s = {s|outputChar = f s.outputChar}

modifyOutputMovement : Setter TFMovement TuringFunction
modifyOutputMovement f s = {s|outputMovement = f s.outputMovement}


type alias Model = {
          states : List State
        , initialState : State
        , acceptedStates : List State
        , delta : List TuringFunction
        , input : String

        , result : TMResult
    }

type TMResult = TMReceived String | TMError H.Error | TMNotRun

modifyDelta : Setter (List TuringFunction) Model
modifyDelta f m = {m|delta = f m.delta}

modifyInput : Setter String Model
modifyInput f m = {m|input = f m.input}

init : () -> (Model, Cmd Msg)
init _ = ({
      states = ["0"]
    , initialState = "q0"
    , acceptedStates = []
    , delta = []
    , input = ""

    , result = TMNotRun
    }, Cmd.none)

subs : Model -> Sub Msg
subs _ = Sub.none

type Msg = NOP
         | UpdateModel Model
         | UpdateStates (List String)
         | UpdateAcceptedStates (List String)
         | Run
         | ReceivedResult (Result H.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
        NOP -> (model, Cmd.none)
        UpdateModel newmodel -> (newmodel, Cmd.none)
        UpdateStates newstates -> ({model|states=newstates,
                initialState=if List.member model.initialState model.states then model.initialState
                else Maybe.withDefault "q0" <| List.head (model.states)
            }, Cmd.none)
        UpdateAcceptedStates newstates -> ({model|acceptedStates=newstates}, Cmd.none)
        Run -> (model, H.post {
              url="runTuring"
            , expect=H.expectString ReceivedResult
            , body=H.jsonBody (encodeModel model)
            })
        ReceivedResult res -> ({model|result = either TMError TMReceived res}, Cmd.none)

view : Model -> Browser.Document Msg
view model = {
          title="Turingmaschinen Simulator"
        , body=
            navbar
         ++ [
                div [A.class "container"] [
                      div [A.class "explanation"] explanation
                    , div [A.class "turingInput"] <| turingInput model
                ]
            ]
    }

navbar : List (Html Msg)
navbar = [
        nav [A.class "navbar"] [
              strong [] [text "Turingmaschinen Simulator"]
            , a [A.href "/about"] [text "Source Code und weitere Tools"]
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

        , h4 [] [text "δ"]
        , table [] [
            tbody [] <| List.indexedMap (\i tf -> tr [] [
                  td [] [text "(", stringSelect [A.value tf.inputState, E.onInput (\s -> UpdateModel (model |> (modifyDelta (mapAt i (modifyInputState (const s))))))] model.states,
                    text ", ", charInput (\s -> UpdateModel (model |> modifyDelta (mapAt i (modifyInputChar (const <| Maybe.withDefault '_' s))))) tf.inputChar, text ")"]
                , td [] [text "→"]
                , td [] [text "(", stringSelect [A.value tf.outputState, E.onInput (\s -> UpdateModel (model |> (modifyDelta (mapAt i (modifyOutputState (const s))))))] model.states,
                    text ", ", charInput  (\s -> UpdateModel (model |> modifyDelta (mapAt i (modifyOutputChar (const <| Maybe.withDefault '_' s))))) tf.outputChar,
                    text ", ", stringSelect [A.value (movementToString tf.outputMovement), E.onInput (\s -> UpdateModel (model |> modifyDelta (mapAt i (modifyOutputMovement (const (movementFromString s))))))] ["Links", "Rechts", "Bleib"]
                    , text ")"]
                , td [] [button [E.onClick (UpdateModel (model |> modifyDelta (removeAt i)))] [text "-"]]
                ]
            ) model.delta ++ [tr [] [button [E.onClick <| UpdateModel (model |> modifyDelta (\x -> x ++ [emptyTF model])) {-Debug.todo "AddTF")-}] [text "+"]]]
        ]
        , input [A.value model.input, E.onInput (\s -> UpdateModel (model |> modifyInput (const s)))] []
        , button [E.onClick Run] [text "Run"]
        , case model.result of
            TMNotRun -> text ""
            TMError e -> h1 [] [text <| "Error: " ++ Debug.toString e]
            TMReceived res -> div [] (List.map (p [] << List.singleton << text) <| String.split "\n" res)
        ]



encodeModel : Model -> JD.Value
encodeModel model = JE.object [
          ("states", JE.list JE.string model.states)
        , ("initialState", JE.string model.initialState)
        , ("acceptedStates", JE.list JE.string model.acceptedStates)
        , ("delta", JE.list encodeTF model.delta)
        , ("input", JE.string model.input)
    ]

encodeTF : TuringFunction -> JD.Value
encodeTF tf = JE.object [
          ("inputState", JE.string tf.inputState)
        , ("inputChar", JE.string <| String.fromChar <| tf.inputChar)
        , ("outputState", JE.string tf.outputState)
        , ("outputChar", JE.string <| String.fromChar <| tf.outputChar)
        , ("outputMovement", JE.string <| movementToString <| tf.outputMovement)
    ]

