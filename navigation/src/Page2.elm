module Page2 exposing (..)

import Browser exposing (Document)
import Html
import Html exposing (div, text)
import Platform.Cmd

import Navigation

type State = State

type Action = Action

initial : State
initial = State

view : State -> Document Action
view state =
  { title = "Page 2"
  , body =
    [ Navigation.navbar Navigation.Page_2
    , div [] [text "Page 2"]
    ]
  }

update : Action -> State -> (State, Cmd Action)
update action State =
  case action of
    Action -> (State, Cmd.none)
