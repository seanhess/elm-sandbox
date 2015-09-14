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

type alias Transition = List Animation

none = []

addTransition : Animation -> Transition -> Transition
addTransition a ts = a :: ts

cleanup : Time -> Transition -> Transition
cleanup time ts = List.filter (not << Animation.isDone time) ts

transitionValue : Time -> Transition -> Float
transitionValue time trans =
  List.map (animate time) trans
    |> List.sum

dump : Time -> Transition -> String
dump time ts = toString <| List.map (animate time) ts

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
            , transition <- addTransition ( anim model.time maxWidth) model.transition
          }

        Closed ->
          { model
            | state <- Open
            , transition <- addTransition ( anim model.time -maxWidth ) model.transition
          }
      in
      ( model'
      , Effects.none )

    Tick t ->
      ( { model | time <- t, transition <- cleanup model.time model.transition }
      , Effects.tick Tick )

anim : Time -> Float -> Animation
anim t s =
  animation t |> from s |> to 0 |> duration (0.8 * second)

-- I need a timer in there
-- effects?
-- update is called every tiny second?
-- or only on a tick

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
                , "trans: " ++ toString (List.length model.transition) ++ " " ++ dump model.time model.transition
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

