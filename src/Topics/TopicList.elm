module Topics.TopicList where

import Effects exposing (Effects, Never)
import Easing exposing (ease, easeOutBounce, easeInQuad, easeInOutQuad, float)

import Html exposing (div, h1, text, Html, a)
import Html.Attributes exposing (style, href)
import Html.Events exposing (onClick)

import Svg exposing (Svg, svg)
import Svg.Attributes exposing (width, height, viewBox)
import Signal exposing (Signal, Address)
import StartApp exposing (App)

import Time exposing (Time, second)

import Task exposing (Task)
import Topics.Types exposing (TopicTerm)
import Topics.TopicRow as TopicRow exposing (RowTransition, indexY)

import Debug

type alias Model =
  { terms : List TopicTerm
  , sort : ListSort
  , oldSort : ListSort
  , transition : Transition
  }


--------------------------------------------------------

type alias Transition =
  { elapsed : Time
  , timer : Timer
  , duration : Time
  , effect : Effects Action
  }

type Timer = Started | Stopped | Running Time

transition : Time -> Transition
transition dur = { elapsed = dur, duration = dur, timer = Stopped, effect = Effects.none}

start : Transition -> Transition
start trans =
  let effect =
        case trans.timer of
          Stopped -> Effects.tick Tick
          _ -> Effects.none
  in { trans | elapsed <- 0, timer <- Started, effect <- effect}

stop : Transition -> Transition
stop trans = { trans | elapsed <- trans.duration, timer <- Stopped, effect <- Effects.none }

run : Transition -> Time -> Transition
run trans newTime =
  let {elapsed, timer} = trans
      newElapsed =
        case timer of
          Running oldTime ->
            elapsed + (newTime - oldTime)
          _ -> 0
  in

  if newElapsed > trans.duration
    then stop trans
    else { trans | elapsed <- newElapsed, timer <- Running newTime, effect <- Effects.tick Tick }

duration : Time
duration = second / 2

offsetY : Int -> Time -> Float
offsetY dIndex elapsed =
  ease easeInOutQuad float (indexY dIndex) 1 duration elapsed


-----------------------------------------------------------

type ListSort = SortAZ | SortScore

type Action = RowAction TopicRow.Action | ChangeSort ListSort | Tick Time

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    RowAction _ -> (model, model.transition.effect)

    ChangeSort sort ->
      let d = Debug.log "sort" sort
          trans = start model.transition
      in
      ( { model
          | sort <- sort
          , oldSort <- model.sort
          , transition <- trans
        }
      , trans.effect
      )

    Tick newTime ->
      let trans = run model.transition newTime
          b = Debug.log "trans" trans
      in

      ( { model | transition <- trans }
      , trans.effect
      )


init : (Model, Effects Action)
init = (emptyModel, Effects.none)

sortTerms : ListSort -> (TopicTerm -> comparable)
sortTerms s =
  case s of
    SortScore -> toString << .topicFrequency
    SortAZ -> .name

emptyModel : Model
emptyModel =
  { sort = SortScore
  , oldSort = SortScore
  , transition = transition duration
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

type alias TermIndex =
  { term : TopicTerm
  , index : Int
  , oldIndex : Int
  }

emptyIndex index term = { term = term, index = index, oldIndex = 0 }
setOldIndex ix termIndex = { termIndex | oldIndex <- ix }

rowModel : Transition -> TermIndex -> TopicRow.Model
rowModel {elapsed, timer} {term, index, oldIndex} =
  { term = term
  , index = index
  , transition = offsetY (oldIndex - index) elapsed
  }

rowModels : Transition -> ListSort -> ListSort -> List TopicTerm -> List TopicRow.Model
rowModels trans oldSort newSort terms =
  let tis = List.indexedMap emptyIndex (List.sortBy (sortTerms newSort) terms)
      byOld = List.sortBy (sortTerms oldSort << .term) tis
      tis' = List.indexedMap (setOldIndex) byOld
  in List.map (rowModel trans) tis'

view : Address Action -> Model -> Html
view address model =
  let models = rowModels model.transition model.oldSort model.sort model.terms
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
        (List.map (row address) models)
    ]

row : Address Action -> TopicRow.Model -> Html
row address model =
  TopicRow.view (Signal.forwardTo address RowAction) model

---------------------------------------------
type alias Style = List (String, String)
clickable : Style
clickable = [("cursor", "pointer")]
