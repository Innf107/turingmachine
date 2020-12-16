{-#LANGUAGE LambdaCase, BlockArguments, OverloadedStrings, TypeApplications#-}
module Main where

import Web.Scotty as S

import Data.Aeson
import qualified Data.Text.Lazy as T

import qualified Elm

import TuringMachine
import Lib ((∈), (∉))
import Data.Maybe (fromJust)


main :: IO ()
main = scotty 4567 $ do
    get "/" $ file "public/index.html"
    get "/about" $ file "public/about.html"
    get "/index.js" $ file "public/index.js"
    get "/main.css" $ file "public/main.css"
    post "/runTuring" $ do
        mmodel <- eitherDecode @Elm.Model <$> body
        model <- case mmodel of
            Left e -> raiseStatus (toEnum 400) $ T.pack $ "Invalid Inputs: " ++ e
            Right x -> return x
        let (steps, accepted) = runTuring (toTuring model) (Elm.input model)
        S.text $ T.pack ("Accepted: " ++ show accepted ++ unlines (map show steps))

toTuring :: Elm.Model -> Turing
toTuring model = Turing {
        initialState = translateState (Elm.initialState model)
      , acceptedStates = map translateState (Elm.acceptedStates model)
      , program = TuringFunction $ flip lookup (toTFMap (Elm.delta model))
    }
    where
        stateMap :: [(String, Int)]
        stateMap = zip (Elm.states model) [0..]
        translateState :: String -> Int
        translateState s = case lookup s stateMap of
            Nothing -> error $ "InvalidState: " ++ show s
            Just x -> x

        toTFMap :: [Elm.TuringFunction] -> [((TState, Char), (TState, Char, TDirection))]
        toTFMap = map (\tf -> (
            (translateState (Elm.inputState tf),
             Elm.inputChar tf),
            (translateState (Elm.outputState tf),
             Elm.outputChar tf,
             Elm.outputMovement tf)))
