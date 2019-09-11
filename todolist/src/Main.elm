import Browser
import Html exposing (Html, a, button, div, form, input, label, node, p, span, text)
import Html.Attributes exposing (class, for, href, id, placeholder, style, type_, value)
import Html.Events exposing
  ( on, onCheck, onClick, onInput, onMouseEnter, onMouseOut, onMouseOver, onSubmit
  , keyCode, targetValue
  )
import List
import String

type Id = Id Int

type alias Item =
  { id : Id
  , content : String
  , done : Bool
  }

type alias State =
  { counter : Int
  , itemField : String
  , items : List Item
  }

type Action
  = Items_ToggleDone Id
  | Items_Add
  | Items_Remove Id

  | ItemField_Update String

control attrs = div (class "control" :: attrs)
box = div [class "box"]
field attrs = div (class "field" :: attrs)
level a = div [class "level"] [a]
levelItem a = div [class "level-item"] [a]

renderItem : Item -> Html Action
renderItem i =
  box
  [ field [class "is-horizontal level"]
    [ control [class "is-expanded level-item"]
        [ input
            ([class "input is-static", value i.content] ++
             (if i.done then [style "text-decoration" "line-through"] else []))
            []
        ]
    , control
        [class "level-item"]
        [ span
            [ class ("icon " ++ (if i.done then "ticked" else "unticked"))
            , onClick (Items_ToggleDone i.id)
            ]
            [node "i" [class "fas fa-lg fa-check"] []]
        ]
    , control
        [class "level-item"]
        [ span
            [ class "icon remove"
            , onClick (Items_Remove i.id)
            ]
          [node "i" [class "fas fa-lg fa-times"] []]
        ]
    ]
  ]

view : State -> Html Action
view st =
  div [class "container", style "width" "55%", style "padding" "0.5em"] <|
    form [style "margin-bottom" "1.5rem", onSubmit Items_Add]
      [ field [class "has-addons"]
        [ control
          [ class "is-expanded" ]
          [ input
            [ type_ "text"
            , class "input"
            , placeholder "Add an item"
            , onInput ItemField_Update
            , value st.itemField
            ]
          []
          ]
        , control [] [input [type_ "submit", class "button is-primary", value "Add"] []]
        ]
      ] ::
    List.map renderItem st.items

update : Action -> State -> State
update msg st =
  case msg of
    Items_ToggleDone id ->
      { st
      | items =
          List.map (\i -> if i.id == id then { i | done = not i.done } else i) st.items
      }
    Items_Add ->
      { st
      | counter = st.counter + 1
      , itemField = ""
      , items =
        { id = Id st.counter
        , content = st.itemField
        , done = False
        } ::
        st.items
      }
    Items_Remove id ->
      { st | items = List.filter (\i -> i.id /= id) st.items }

    ItemField_Update val -> { st | itemField = val }

main : Program () State Action
main =
  Browser.sandbox
  { init = { counter = 0, itemField = "", items = [] }
  , view = view
  , update = update
  }
