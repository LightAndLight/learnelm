import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Browser.Navigation exposing (Key)
import List
import Html exposing (Attribute, Html, a, nav, text)
import Html.Attributes exposing (href, style)
import Platform.Cmd exposing (Cmd)
import Platform.Cmd as Cmd
import Platform.Sub exposing (Sub)
import Platform.Sub as Sub
import Url exposing (Url)
import Url.Builder as Url

import Navigation
import Home
import Page1
import Page2

mapDocument : (a -> b) -> Document a -> Document b
mapDocument f d =
  { title = d.title
  , body = List.map (Html.map f) d.body
  }

type PageState
  = State_Home Home.State
  | State_Page1 Page1.State
  | State_Page2 Page2.State
  | State_Page404

type alias State = { key : Key, page : PageState }

type Action
  = Nav_To Navigation.Page
  | Nav_Load Navigation.Page
  | Nav_External String
  | Action_Home Home.Action
  | Action_Page1 Page1.Action
  | Action_Page2 Page2.Action

init : () -> Url -> Key -> (State, Cmd Action)
init () url key =
  case Navigation.urlPage url of
    Navigation.Page_Home -> ({ key = key, page = State_Home Home.initial }, Cmd.none)
    Navigation.Page_1 -> ({ key = key, page = State_Page1 Page1.initial }, Cmd.none)
    Navigation.Page_2 -> ({ key = key, page = State_Page2 Page2.initial }, Cmd.none)
    Navigation.Page_404 -> ({ key = key, page = State_Page404 }, Cmd.none)

view : State -> Document Action
view state =
  case state.page of
    State_Home homeState ->
      mapDocument Action_Home (Home.view homeState)
    State_Page1 page1State ->
      mapDocument Action_Page1 (Page1.view page1State)
    State_Page2 page2State ->
      mapDocument Action_Page2 (Page2.view page2State)
    State_Page404 ->
      { title = "Page not found", body = [text "not found"] }

update : Action -> State -> (State, Cmd Action)
update action state =
  case action of
    Nav_To page ->
      case page of
        Navigation.Page_Home ->
          ( state
          , Nav.pushUrl state.key <| Url.absolute [] []
          )
        Navigation.Page_1 ->
          ( state
          , Nav.pushUrl state.key <| Url.absolute ["page1"] []
          )
        Navigation.Page_2 ->
          ( state
          , Nav.pushUrl state.key <| Url.absolute ["page2"] []
          )
        Navigation.Page_404 ->
          ( state
          , Nav.pushUrl state.key <| Url.absolute ["404"] []
          )
    Nav_Load page ->
      case page of
        Navigation.Page_Home ->
          ( { state | page = State_Home Home.initial }
          , Cmd.none
          )
        Navigation.Page_1 ->
          ( { state | page = State_Page1 Page1.initial }
          , Cmd.none
          )
        Navigation.Page_2 ->
          ( { state | page = State_Page2 Page2.initial }
          , Cmd.none
          )
        Navigation.Page_404 ->
          ( { state | page = State_Page404 }
          , Cmd.none
          )
    Nav_External url -> (state, Nav.load url)
    Action_Home homeAction ->
      case state.page of
        State_Home homeState ->
          case Home.update homeAction homeState of
            (newState, cmd) ->
              ({ state | page = State_Home newState }, Cmd.map Action_Home cmd)
        _ -> (state, Cmd.none)
    Action_Page1 page1Action ->
      case state.page of
        State_Page1 page1State ->
          case Page1.update page1Action page1State of
            (newState, cmd) ->
              ({ state | page = State_Page1 newState }, Cmd.map Action_Page1 cmd)
        _ -> (state, Cmd.none)
    Action_Page2 page2Action ->
      case state.page of
        State_Page2 page2State ->
          case Page2.update page2Action page2State of
            (newState, cmd) ->
              ({ state | page = State_Page2 newState }, Cmd.map Action_Page2 cmd)
        _ -> (state, Cmd.none)

subscriptions : State -> Sub Action
subscriptions _ = Sub.none

onUrlRequest : UrlRequest -> Action
onUrlRequest urlReq =
  case urlReq of
    Internal url -> Nav_To (Navigation.urlPage url)
    External url -> Nav_External url

onUrlChange : Url -> Action
onUrlChange = Nav_Load << Navigation.urlPage

main : Program () State Action
main =
  Browser.application
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  , onUrlRequest = onUrlRequest
  , onUrlChange = onUrlChange
  }
