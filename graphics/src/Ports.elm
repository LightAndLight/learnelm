port module Ports exposing (..)

import Platform.Cmd exposing (Cmd)

port log : String -> Cmd msg
