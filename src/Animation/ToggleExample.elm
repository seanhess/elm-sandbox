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
import Animation.Transition as Trans exposing (Transition, additive, add)


type PageState = Open | Closed
type alias Model =
  { state : PageState
  , time : Time
  , trans : Transition
  }

init : ( Model, Effects Action )
init =
  ( { state = Closed
    , trans = Trans.none
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
            , trans <- add (additive model.time maxWidth 0) model.trans
          }

        Closed ->
          { model
            | state <- Open
            , trans <- add (additive model.time 0 maxWidth) model.trans
          }
      in
      ( model'
      , Effects.none )

    Tick t ->
      ( { model | time <- t }
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
                , Trans.dump model.time model.trans
                ]
            ]
        ]
    ]

maxWidth = 200.0

boxWidth : Model -> Float
boxWidth m =
  let a = Trans.animate m.time m.trans in
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

