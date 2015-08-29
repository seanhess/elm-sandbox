module Topics.TopicRow where

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Signal exposing (Signal, Address)
import Topics.Types exposing (TopicTerm)

import Time exposing (Time, second)


rowHeight : Int
rowHeight = 25

type Action = Noop
type alias Model =
  { term : TopicTerm
  , index: Int
  , transition : RowTransition
  }

type alias RowTransition = Float

update : Action -> Model -> Model
update _ model = model

-- they aren't independent 
view : Address Action -> Model -> Svg
view address ({term, index, transition}) =
  let y' = indexY index + transition
      x' = 100
  in
  g [ transform (translate x' y') ]
    [ g []
        [ bar term ]
    , text'
        [ textAnchor "end", x "0", y (toString ((toFloat rowHeight) / 2.0 + 2.0))]
        [ text term.name ]
    ]

bar : TopicTerm -> Svg
bar { overallFrequency, topicFrequency } =
  let max = 200
      overallWidth = overallFrequency * max
      topicWidth = topicFrequency * max
  in
  g [ transform (translate 10 0) ]
    [ rect
        [ width (toString overallWidth)
        , height (toString (rowHeight - 6))
        , fill "#A5C8E1"
        , stroke "#85A8D1"
        ] []
    , rect
        [ width (toString topicWidth)
        , height (toString (rowHeight - 6))
        , fill "#CC474D"
        , stroke "#CC474D"
        ] []
    ]

indexY : Int -> Float
indexY index = toFloat (index * rowHeight)

translate : a -> a -> String
translate x y = "translate(" ++ toString x ++ "," ++ toString y ++ ")"

