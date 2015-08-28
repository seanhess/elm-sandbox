module CardItem where

-- you know, using a full-on FRP system might make more sense anyway
-- it's probably a really good fit for this domain
-- are animations easier with an FRP system?

-- I'm going to play around with elm FRP stuff. like games?

import Easing exposing (ease, easeOutBounce, float)
import Effects exposing (Effects)
import Time exposing (Time, second)
import Card exposing (Card)
import Html exposing (div, text, Html)
import Signal exposing (Address)
import Html.Attributes exposing (..)

type alias AnimationState =
    Maybe { prevClockTime : Time,  elapsedTime: Time }

type alias Model =
    { index : Int
    , offset : Int
    , animationState : AnimationState
    , card : Card
    }

init : Card -> (Model, Effects Action)
init card =
    ( { index = 0, offset = 0, animationState = Nothing, card = card}
    , Effects.none
    )

duration : Time
duration = second

-- there aren't any actions to perform here
-- it just resorts
type Action = Tick Time

-- I need to say "your y CHANGED", not just animate it :)
update : Action -> Model -> (Model, Effects Action)
update msg model =
  case msg of
    Tick clockTime ->
      let y = model.index * 50
          newElapsedTime =
            case model.animationState of
              Nothing -> 0
              Just { elapsedTime, prevClockTime } ->
                elapsedTime + (clockTime - prevClockTime)
      in
          if newElapsedTime > duration then
            ( { model
                  | offset <- y
                  , animationState <- Nothing
              }
            , Effects.none
            )
          else
            ( { model
                  | animationState <- Just { elapsedTime = newElapsedTime, prevClockTime = clockTime }
              }
            , Effects.tick Tick
            )

toOffset : AnimationState -> Float
toOffset animationState =
  case animationState of
    Nothing -> 0
    Just {elapsedTime} ->
      ease easeOutBounce float 0 100 duration elapsedTime

view : Address Action -> Model -> Html
view address model =
  let x = 100
      card = model.card
      y = toFloat (50 + (model.index * 40)) + toOffset model.animationState
      tm = "translate(" ++ toString x ++ "px," ++ toString y ++ "px) rotate(20deg)"
  in
  div
    [ key (toString card.id)
    , style
        [ ("width","100")
        , ("height","100")
        , ("padding", "10px")
        , ("color", "white")
        , ("transition", "transform 2s")
        , ("transform", tm)
        , ("background","blue")
        , ("position","absolute")
        , ("border", "solid 1px white")
        ]
    ]
    [ text card.name ]

