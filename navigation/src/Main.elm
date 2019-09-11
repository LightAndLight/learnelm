import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Browser.Navigation exposing (Key)
import Html exposing (Attribute, Html, a, nav, text)
import Html.Attributes exposing (href, style)
import Platform.Cmd exposing (Cmd)
import Platform.Cmd as Cmd
import Platform.Sub exposing (Sub)
import Platform.Sub as Sub
import Url exposing (Url)

type Page
  = Page_Home
  | Page_1
  | Page_2
  | Page_404

pageTitle : Page -> String
pageTitle page =
  case page of
    Page_Home -> "Home"
    Page_1 -> "Page 1"
    Page_2 -> "Page 2"
    Page_404 -> "Page not found"

urlPage : Url -> Page
urlPage url =
  case url.path of
    "/" -> Page_Home
    "/page1" -> Page_1
    "/page2" -> Page_2
    _ -> Page_404

type alias State = { page : Page }

type Action
  = Nav_Internal Page
  | Nav_External String

init : () -> Url -> Key -> (State, Cmd Action)
init () url key = ({ page = urlPage url }, Cmd.none)

currentNav : Page -> Page -> List (Attribute Action)
currentNav current this =
  if this == current
  then [style "text-decoration" "underline"]
  else []

navbar : Page -> Html Action
navbar page =
  nav []
    [ a ([href "/"] ++ currentNav page Page_Home) [text "Home"]
    , a ([href "/page1"] ++ currentNav page Page_1) [text "Page 1"]
    , a ([href "/page2"] ++ currentNav page Page_2) [text "Page 2"]
    ]

view : State -> Document Action
view state =
  { title = pageTitle state.page
  , body =
    [ navbar state.page
    ]
  }

update : Action -> State -> (State, Cmd Action)
update action state =
  case action of
    Nav_Internal page -> ({ state | page = page }, Cmd.none)
    Nav_External url -> (state, Nav.load url)

subscriptions : State -> Sub Action
subscriptions _ = Sub.none

onUrlRequest : UrlRequest -> Action
onUrlRequest urlReq =
  case urlReq of
    Internal url -> Nav_Internal (urlPage url)
    External url -> Nav_External url

onUrlChange : Url -> Action
onUrlChange = Nav_External << Url.toString

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
