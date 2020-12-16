{-#LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings#-}
module Elm where

import GHC.Generics (Generic)

import Data.Aeson as A

import TuringMachine (TDirection(..))

data TuringFunction = TuringFunction {
          inputState :: String
        , inputChar :: Char
        , outputState :: String
        , outputChar :: Char
        , outputMovement :: TDirection
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromJSON TDirection where
    parseJSON (A.String "Links") = pure TLeft
    parseJSON (A.String "Rechts") = pure TRight
    parseJSON (A.String "Bleib") = pure TStay
    parseJSON _ = fail "Expected 'Links', 'Rechts', or 'Bleib'"

instance ToJSON TDirection where
    toJSON TLeft = A.String "Links"
    toJSON TRight = A.String "Right"
    toJSON TStay = A.String "Bleib"

data Model = Model {
        states::[String]
      , initialState::String
      , acceptedStates::[String]
      , delta::[TuringFunction]
      , input::String
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)


