module Home exposing (..)

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
  { title = "Home"
  , body =
    [ Navigation.navbar Navigation.Page_Home
    , div [] [text "Home page"]
    ]
  }

update : Action -> State -> (State, Cmd Action)
update action State =
  case action of
    Action -> (State, Cmd.none)
