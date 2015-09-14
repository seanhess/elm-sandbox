module Animation.ToggleExample where

import Html exposing (div, button, text, Html, pre)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp as StartApp
import Signal exposing (Address)
import Animation exposing (animation, from, to, duration, delay, Animation, static, animate)
import Time exposing (Time, second)
import Effects exposing (Effects, Never)
import Task exposing (Task)
import List
import String

--------------------------------------------------------------------

type alias Transition =
  { animations : List Animation
  , time : Time
  }

none = { animations = [], time = 0 }

add : Animation -> Transition -> Transition
add a trans = { trans | animations <- a :: trans.animations }

-- should always be zero
-- should go from max value
-- have good defaults and easing

tick : Time -> Transition -> Transition
tick time trans =
  let anims = List.filter (not << Animation.isDone time) trans.animations
  in { trans | animations <- anims
             , time <- time
     }

transitionValue : Time -> Transition -> Float
transitionValue time trans =
  List.sum <| List.map (animate time) trans.animations

dump : Time -> Transition -> String
dump time trans = toString <| List.map (animate time) trans.animations

anim : Time -> Float -> Animation
anim t s =
  animation t |> from s |> to 0 |> duration (0.8 * second)


--------------------------------------------------------------------

type PageState = Open | Closed
type alias Model =
  { state : PageState
  , time : Time
  , transition : Transition
  }

init : ( Model, Effects Action )
init =
  ( { state = Closed
    , transition = none
    , time = 0
    }
  , Effects.tick Tick )

-----------------------------------------------------------------------


type Action
  = Toggle
  | Tick Time

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Toggle ->
      let model' = case model.state of
        Open ->
          { model
            | state <- Closed
            , transition <- add (anim model.time maxWidth) model.transition
          }

        Closed ->
          { model
            | state <- Open
            , transition <- add (anim model.time -maxWidth) model.transition
          }
      in
      ( model'
      , Effects.none )

    Tick t ->
      ( { model | time <- t, transition <- tick model.time model.transition }
      , Effects.tick Tick )

-----------------------------------------------------------------------------

view : Address Action -> Model -> Html
view address model =
  div [ style [("margin", "10px")] ]
    [ div [ style [("margin-bottom", "10px")] ]
        [ button
            [ style buttonStyle
            , onClick address Toggle ]
            [ text "Toggle" ]
        ]
    , div
        [ style
            [ ("width", (toString (boxWidth model) ++ "px"))
            , ("height", "150px")
            , ("background", "#E6E6EF")
            , ("border-right", "solid 10px #716DCF")
            ]
        ]
        []
    , div
        []
        [ pre []
            [ text <| String.join "\n"
                [ "time: " ++ toString (round (model.time / 1000))
                , "trans: " ++ toString (List.length model.transition.animations) ++ " " ++ dump model.time model.transition
                ]
            ]
        ]
    ]

maxWidth = 200.0

boxWidth : Model -> Float
boxWidth m =
  let a = transitionValue m.time m.transition in
  case m.state of
    Open -> maxWidth + a
    Closed -> 0.0 + a

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

