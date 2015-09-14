module Animation.ClickExample where

import Html exposing (div, button, text, Html, pre, p)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp as StartApp
import Signal exposing (Address)
import Animation exposing (animation, from, to, duration, delay, Animation, static, animate)
import Time exposing (Time, second)
import Effects exposing (Effects, Never)
import Task exposing (Task)
import List
import String
import Json.Decode as Json exposing ((:=))

import Animation.Transition as Transition exposing (Transition)


type PageState = Open | Closed
type alias Model =
  { state : PageState
  , time : Time
  , transX : Transition
  , transY : Transition
  , position : Position
  }

type alias Position =
  { x : Float
  , y : Float
  }

init : ( Model, Effects Action )
init =
  ( { state = Closed
    , transX = Transition.none
    , transY = Transition.none
    , time = 0
    , position = Position 10 10
    }
  , Effects.tick Tick )

-----------------------------------------------------------------------


type Action
  = Tick Time
  | Click Position

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Tick t ->
      ( { model | time <- t }
      , Effects.tick Tick )

    Click pos ->
      let dx = (model.position.x - pos.x)
          dy = (model.position.y - pos.y)
          transX = Transition.add (Transition.additive model.time dx) model.transX
          transY = Transition.add (Transition.additive model.time dy) model.transY
      in
      ( { model | position <- pos, transX <- transX, transY <- transY }
      , Effects.none )

-----------------------------------------------------------------------------

view : Address Action -> Model -> Html
view address model =
  div [ style [] ]
    [ div
        [ style
            [ ("width", "400px")
            , ("height", "400px")
            , ("background", "#E6E6EF")
            , ("position", "relative")
            , ("overflow", "hidden")
            ]
        , on "click" clickPosition (Signal.message address << Click)
        ]
        [ box model ]
    , p [] [ text "Click anywhere in the box" ]
    , div
        []
        [ pre []
            [ text <| String.join "\n"
                [ "time: " ++ toString (round (model.time / 1000))
                -- , Transition.dump model.time model.transX
                ]
            ]
        ]
    ]

boxSize = 40

box : Model -> Html
box model =
  let top = model.position.y - boxSize/2 + Transition.animate model.time model.transY
      left = model.position.x - boxSize/2 + Transition.animate model.time model.transX
  in
  div
    [ style
      [ ("background", "#716DCF")
      , ("width", "40px")
      , ("height", "40px")
      , ("top", toString top ++ "px")
      , ("left", toString left ++ "px")
      , ("position", "absolute")
      ]
    ]
    []


buttonStyle =
  [ ( "color", "#555" )
  , ( "text-transform", "uppercase" )
  , ( "cursor", "pointer" )
  , ( "letter-spacing", "0.15em" )
  , ( "padding", "8px" )
  , ( "border", "solid 2px #555" )
  , ( "background", "white" )
  , ( "borderRadius", "6px" )
  , ( "fontWeight", "bold" )
  , ( "font", "11px 'HelveticaNeue'" )
  , ( "outline", "none" )
  ]

------------------------------------------------------------------------------

app =
  StartApp.start { init = init, view = view, update = update, inputs = [] }

main : Signal Html
main = app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks


----------------------------------------------

clickPosition : Json.Decoder Position
clickPosition =
  Json.object2 Position
    ("clientX" := Json.float)
    ("clientY" := Json.float)

