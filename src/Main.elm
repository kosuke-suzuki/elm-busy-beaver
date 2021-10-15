module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser
import Time
import Array

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Alphabet = O | I
toChar : Alphabet -> Char
toChar alpha =
  case alpha of
    O ->
      '0'
    I ->
      '1'

type Direction = Left | Right

type alias StepOutput = 
  { alphabet : Alphabet
  , direction : Direction
  , state : State
  }

type State = Halted | Running (Alphabet -> StepOutput)

stepState : State -> Alphabet -> Maybe StepOutput
stepState state alpha = case state of
  Running f ->
    Just <| f alpha
  Halted ->
    Nothing

aState : State
aState = Running (\alpha ->
  case alpha of
    O ->
      { alphabet = I
      , direction = Right
      , state = bState
      }
    I ->
      { alphabet = I
      , direction = Left
      , state = bState
      }
  )

bState : State
bState = Running (\alpha ->
  case alpha of
    O ->
      { alphabet = I
      , direction = Left
      , state = aState
      }
    I ->
      { alphabet = O
      , direction = Left
      , state = cState
      }
  )

cState : State
cState = Running (\alpha ->
  case alpha of
    O ->
      { alphabet = I
      , direction = Right
      , state = Halted
      }
    I ->
      { alphabet = I
      , direction = Left
      , state = dState
      }
  )

dState : State
dState = Running (\alpha ->
  case alpha of
    O ->
      { alphabet = I
      , direction = Right
      , state = dState
      }
    I ->
      { alphabet = O
      , direction = Right
      , state = aState
      }
  )

type alias Tape = (List Alphabet, Int)

currentAlphabet : Tape -> Alphabet
currentAlphabet tape =
  let
    alphaList = Tuple.first tape
    index = Tuple.second tape
  in
    Array.fromList alphaList
    |> Array.get (index - 1) 
    |> Maybe.withDefault I

reflectToTape : Tape -> StepOutput -> Tape
reflectToTape tape stepOutput =
  let
    currentList = Tuple.first tape
    currentIndex = Tuple.second tape
    nextIndex = 
      case stepOutput.direction of
        Left ->
          currentIndex - 1
        Right ->
          currentIndex + 1
    nextList = 
      List.drop currentIndex currentList
      |> List.append [stepOutput.alphabet]
      |> List.append (List.take (currentIndex - 1) currentList)
  in
    ( nextList
    , nextIndex
    )

type alias Model =
  { isRunning : Bool
  , state : State
  , tape : Tape
  }

init : () -> (Model, Cmd Msg)
init _ = 
  ( { isRunning = False
    , state = aState
    , tape = (List.repeat 20 O, 15)
    }
  , Cmd.none
  )

reflectToModel : Model -> StepOutput -> Model
reflectToModel model stepOutput =
  { model |
    tape = reflectToTape model.tape stepOutput
  , state = stepOutput.state
  }

step : Model -> Model
step model =
  let
    alphabet = currentAlphabet model.tape
    next = stepState model.state alphabet
  in
    case next of
      Nothing ->
        { model |
          isRunning = False
        , state = Halted
        }
      Just running ->
        reflectToModel model running


type Msg
  = ToggleRun
  | Step Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ToggleRun ->
      if model.isRunning then
        ( { model | isRunning = False }
        , Cmd.none
        )
      else
        ( { model | isRunning = True }
        , Cmd.none
        )
    Step _ ->
      if not model.isRunning then
        (model, Cmd.none)
      else
        (step model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Time.every 500 Step

view : Model -> Html Msg
view model =
  div []
    [ div []
      [ button [ onClick ToggleRun ]
        [ viewStartStop model
        ]
      ],
      div []
      [
        pre []
        [ viewTape model.tape ]
      ]
    ]

viewStartStop : Model -> Html Msg
viewStartStop model =
  text <| if model.isRunning then
    "Stop"
  else
    "Start"

viewTape : Tape -> Html Msg
viewTape tape =
  (Tuple.first tape |> List.map toChar |> String.fromList)
  ++ "\n"
  ++ String.padLeft (Tuple.second tape) ' ' "^"
  |> text
