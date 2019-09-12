module Navigation exposing (..)

import Html exposing (Attribute, Html, a, nav, text)
import Html.Attributes exposing (href, style)
import Url exposing (Url)

type Page
  = Page_Home
  | Page_1
  | Page_2
  | Page_404

urlPage : Url -> Page
urlPage url =
  case url.path of
    "/" -> Page_Home
    "/page1" -> Page_1
    "/page2" -> Page_2
    _ -> Page_404

currentNav : Page -> Page -> List (Attribute a)
currentNav current this =
  if this == current
  then [style "text-decoration" "underline"]
  else []

navbar : Page -> Html a
navbar page =
  nav []
    [ a ([href "/"] ++ currentNav page Page_Home) [text "Home"]
    , a ([href "/page1"] ++ currentNav page Page_1) [text "Page 1"]
    , a ([href "/page2"] ++ currentNav page Page_2) [text "Page 2"]
    ]
