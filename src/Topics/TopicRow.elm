module Topics.TopicRow where

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Signal exposing (Signal, Address)
import Topics.Types exposing (TopicTerm)

rowHeight : Int
rowHeight = 25

type Action = Noop
type alias Model =
  { term : TopicTerm
  , index: Int
  }

update : Action -> Model -> Model
update _ model = model

-- ok, so what do we want here?
-- a label, and an svg document with it's own magic sauce going on
-- actually we should probably use SVG for all of this.
view : Address Action -> Model -> Svg
view address ({term, index}) =
  let y' = index * rowHeight
      x' = 100
  in
  g [ transform (translate x' y') ]
    [ g []
        [ bar term ]
    , text'
        [ textAnchor "end", x "0", y (toString ((toFloat rowHeight) / 2.0))]
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

translate : a -> a -> String
translate x y = "translate(" ++ toString x ++ "," ++ toString y ++ ")"
