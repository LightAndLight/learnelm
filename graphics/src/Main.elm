import Browser
import Browser exposing (Document)
import Browser.Events exposing (onAnimationFrame, onMouseMove, onKeyDown, onKeyUp)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (width, for, height, id, min, max, type_, value, step, style)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector2 as Vec2
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as Vec3
import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as Mat4
import Platform.Cmd as Cmd
import String
import Time exposing (Posix)
import WebGL exposing (Shader)
import WebGL as GL

import Ports

type alias Model =
  { lastTick : Maybe Int -- milliseconds since epoch
  , rotationRate : Float -- radians / millisecond
  , rotationRateFactor : Float -- scalar
  , angle : Float -- radians

  , sensitivity : Float
  , looking : Bool
  , lastMouse : Maybe (Float, Float)
  , cameraDir : Vec3

  , position : Vec3
  , speed : Float -- units / millisecond
  , forward : Bool
  , backward : Bool
  , left : Bool
  , right : Bool
  }

type Msg
  = Tick Posix
  | SetRotationRate String

  | KeyDown String
  | KeyUp String

  | SetLooking
  | MouseMove Float Float

main : Program () Model Msg
main =
  Browser.document
  { init = \_ -> (init, Cmd.none)
  , view = view
  , update = update
  , subscriptions = \model ->
    Sub.batch <|
      [ onAnimationFrame Tick
      , onKeyDown (Decode.map KeyDown <| Decode.field "key" Decode.string)
      , onKeyUp (Decode.map KeyUp <| Decode.field "key" Decode.string)
      ] ++
      (if model.looking
       then
         [ onMouseMove <|
           Decode.map2 MouseMove
             (Decode.field "clientX" Decode.float)
             (Decode.field "clientY" Decode.float)
         ]
       else [])
  }

init : Model
init =
  { lastTick = Nothing
  , rotationRateFactor = 1
  , rotationRate = 2 * pi / 1000
  , angle = 0

  , sensitivity = 0.01
  , looking = False
  , lastMouse = Nothing
  , cameraDir = vec3 0 0 -5

  , position = vec3 0 0 0
  , speed = 0.001
  , forward = False
  , backward = False
  , left = False
  , right = False
  }

vertexShader : Shader {r | pos : Vec2} {s | look : Mat4, persp : Mat4} {}
vertexShader =
  [glsl|
    attribute vec2 pos;
    uniform mat4 persp;
    uniform mat4 look;

    void main() {
      gl_Position = persp * look * vec4(pos, -5, 1.0);
    }
  |]

fragmentShader : Shader a b {}
fragmentShader =
  [glsl|
    void main() {
      gl_FragColor = vec4(0, 1, 0, 1);
    }
  |]

rotate : Float -> Vec2 -> Vec2
rotate angle v =
  let
    sinAngle = sin angle
    cosAngle = cos angle
  in
    vec2
      (Vec2.getX v * cosAngle - Vec2.getY v * sinAngle)
      (Vec2.getX v * sinAngle + Vec2.getY v * cosAngle)

perspective : Mat4
perspective = Mat4.makePerspective 70 (640 / 480) 1 256

lookAt : Vec3 -> Vec3 -> Mat4
lookAt pos dir = Mat4.makeLookAt pos (Vec3.add pos <| dir) Vec3.j

view : Model -> Document Msg
view model =
  { title = "Graphics"
  , body =
    [ div []
      [ GL.toHtml
        [width 640, height 480, onClick SetLooking]
        [ GL.entity
            vertexShader
            fragmentShader
            (GL.triangleStrip
             [ { pos = rotate model.angle (vec2 -0.5 -0.25) }
             , { pos = rotate model.angle (vec2 0.5 -0.25) }
             , { pos = rotate model.angle (vec2 0 0.25) }
             ])
            { look = lookAt model.position model.cameraDir
            , persp = perspective
            }
        ]
      ]
    , div []
      [ label [for "rotationRate"] [text "Rotation rate"]
      , input
        [ type_ "range"
        , id "rotationRate"
        , style "vertical-align" "middle"
        , min "0", max "1"
        , value (String.fromFloat model.rotationRateFactor)
        , step "0.05"
        , onInput SetRotationRate
        ] []
      ]
    ]
  }

zero3 : Vec3
zero3 = vec3 0 0 0

forward : Bool -> Vec3 -> Vec3
forward b front =
  if b
  then Vec3.normalize <| Vec3.setY 0 front
  else zero3

backward : Bool -> Vec3 -> Vec3
backward b front =
  if b
  then Vec3.negate <| Vec3.normalize <| Vec3.setY 0 front
  else zero3

left : Bool -> Vec3 -> Vec3
left b front =
  if b
  then Vec3.negate <| Vec3.normalize <| Vec3.cross front Vec3.j
  else zero3

right : Bool -> Vec3 -> Vec3
right b front =
  if b
  then Vec3.normalize <| Vec3.cross front Vec3.j
  else zero3

showVec3 : Vec3 -> String
showVec3 v =
  "<" ++
  String.fromFloat (Vec3.getX v) ++ ", " ++
  String.fromFloat (Vec3.getY v) ++ ", " ++
  String.fromFloat (Vec3.getZ v) ++
  ">"

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    SetRotationRate str ->
      case String.toFloat str of
        Nothing -> (model, Cmd.none)
        Just factor -> ({ model | rotationRateFactor = factor }, Cmd.none)
    KeyDown k ->
      case k of
        "a" -> ({ model | left = True }, Cmd.none)
        "d" -> ({ model | right = True }, Cmd.none)
        "w" -> ({ model | forward = True }, Cmd.none)
        "s" -> ({ model | backward = True }, Cmd.none)
        "Escape" -> ({ model | looking = False }, Cmd.none)
        _ -> (model, Cmd.none)
    KeyUp k ->
      case k of
        "a" -> ({ model | left = False }, Cmd.none)
        "d" -> ({ model | right = False }, Cmd.none)
        "w" -> ({ model | forward = False }, Cmd.none)
        "s" -> ({ model | backward = False }, Cmd.none)
        _ -> (model, Cmd.none)
    SetLooking -> ({ model | looking = True }, Cmd.none)
    MouseMove x y ->
      case model.lastMouse of
        Nothing -> ({ model | lastMouse = Just (x, y) }, Cmd.none)
        Just (oldX, oldY) ->
          let
            xdiff = x - oldX
            ydiff = y - oldY
          in
            ( { model
              | lastMouse = Just (x, y)
              , cameraDir =
                  Vec3.scale 5 <|
                  Vec3.normalize <|
                  Vec3.add (Vec3.scale model.sensitivity <| vec3 xdiff (-ydiff) 0) <|
                  model.cameraDir
              }
            , Cmd.none
            )
    Tick time ->
      case model.lastTick of
        Nothing -> ({ model | lastTick = Just (Time.posixToMillis time) }, Cmd.none)
        Just oldTime ->
          let
            newTime = Time.posixToMillis time
            diff = toFloat <| newTime - oldTime
          in
          ( { model
            | angle =
                model.angle +
                model.rotationRate * model.rotationRateFactor * diff
            , position =
                if List.any (\x -> x) [model.left, model.right, model.forward, model.backward]
                then
                  Vec3.add model.position <|
                  Vec3.scale (model.speed * diff) <|
                  Vec3.normalize <|
                  List.foldr Vec3.add zero3
                    [ forward model.forward model.cameraDir
                    , backward model.backward model.cameraDir
                    , left model.left model.cameraDir
                    , right model.right model.cameraDir
                    ]
                else model.position
            , lastTick = Just newTime
            }
          , Cmd.none
          )
