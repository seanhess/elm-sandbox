module Topics.TopicList where

import Html exposing (div, h1, text, Html, a)
import Html.Attributes exposing (style, href)
import Html.Events exposing (onClick)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (width, height, viewBox)

import Signal exposing (Signal, Address)
import StartApp

import Topics.Types exposing (TopicTerm)
import Topics.TopicRow as TopicRow

type alias Model =
  { terms : List TopicTerm
  , sort : ListSort }

type ListSort = SortAZ | SortScore

type Action = RowAction TopicRow.Action | ChangeSort ListSort

update : Action -> Model -> Model
update action model =
  case action of
    RowAction _ -> model
    ChangeSort sort ->
      { model | sort <- sort }

main : Signal Html
main = StartApp.start { model = emptyModel, view = view, update = update }

sortTerms : ListSort -> (TopicTerm -> comparable)
sortTerms s =
  case s of
    SortScore -> toString << .overallFrequency
    SortAZ -> .name

emptyModel : Model
emptyModel =
  { sort = SortScore
  , terms =
      [ TopicTerm "one" 0.1 1
      , TopicTerm "two" 0.2 1
      , TopicTerm "three" 0.3 1
      , TopicTerm "four" 0.4 1
      , TopicTerm "five" 0.5 1
      , TopicTerm "six" 0.6 1
      , TopicTerm "seven" 0.7 1
      , TopicTerm "eight" 0.8 1
      , TopicTerm "nine" 0.9 1
      , TopicTerm "ten" 1 1
      ]
  }


view : Address Action -> Model -> Html
view address model =
  let terms = List.sortBy (sortTerms model.sort) model.terms
  in
  div []
    [ h1 [] [ text "topics" ]
    , div []
        [ a [ style clickable, onClick address (ChangeSort SortAZ) ] [ text "Sort A-Z" ]
        , text " | "
        , a [ style clickable, onClick address (ChangeSort SortScore) ] [ text "Sort Score" ]
        ]
    , svg
        [ width "400", height "500", viewBox "0 0 400 400" ]
        (List.indexedMap (row address) terms)
    ]

row : Address Action -> Int -> TopicTerm -> Html
row address index term =
  TopicRow.view (Signal.forwardTo address RowAction) (termModel index term)

termModel : Int -> TopicTerm -> TopicRow.Model
termModel index term = { term = term, index = index }


---------------------------------------------
type alias Style = List (String, String)
clickable : Style
clickable = [("cursor", "pointer")]
