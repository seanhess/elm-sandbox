module Topics.TopicList where

import Animation exposing (..)
import Effects exposing (Effects, Never)
-- import Easing exposing (ease, easeOutBounce, easeInQuad, easeInOutQuad, float)

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

import List.Extra as List
import Maybe exposing (withDefault)

type alias Model =
  { terms : List TopicTerm
  , sort : ListSort
  , oldSort : ListSort
  , transition : Transition
  }

--------------------------------------------------------

-- it's the animation and the current percent
-- ah, the problem is starting / stopping
type Transition = Transition (Maybe Animation) Float

start : Transition
start = Transition Nothing 1

stop : Transition
stop = Transition Nothing 0

-----------------------------------------------------------

type ListSort = SortAZ | SortScore

type Action = RowAction TopicRow.Action | ChangeSort ListSort | Tick Time

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    RowAction _ -> (model, Effects.none)

    ChangeSort sort ->
      -- don't do anything if we haven't changed
      if sort == model.sort then
        (model, Effects.none)

      else
      ( { model
          | sort <- sort
          , oldSort <- model.sort
          , transition <- start
        }
      , Effects.tick Tick
      )

    Tick newTime ->
      case model.transition of
        Transition Nothing 1 ->
          let anim = animateIndex newTime in
          ( { model | transition <- Transition (Just anim) (animate newTime anim) }
          , Effects.tick Tick
          )

        Transition _ 0 ->
          ( { model | transition <- Transition Nothing 0 }
          , Effects.none
          )

        Transition (Just anim) _ ->
          ( { model | transition <- Transition (Just anim) (animate newTime anim) }
          , Effects.tick Tick
          )



animateIndex : Time -> Animation
animateIndex time =
  animation time |> from 1 |> to 0 |> duration (second/2)

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
  , transition = stop
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

rowModel : Float -> Int -> Int -> TopicTerm -> TopicRow.Model
rowModel pct oldIndex index term =
  { term = term
  , index = index
  , transition = pct * (indexY (oldIndex - index))
  }

rowModels : Transition -> ListSort -> ListSort -> List TopicTerm -> List TopicRow.Model
rowModels (Transition _ pct) oldSort newSort terms =
  let newIxs = sortIndexes newSort terms
      oldIxs = sortIndexes oldSort terms
  in List.map3 (rowModel pct) oldIxs newIxs terms

termIndex : List TopicTerm -> TopicTerm -> Int
termIndex sorted term = withDefault -1 <| List.findIndex (\t -> t == term) sorted

sortIndexes : ListSort -> List TopicTerm -> List Int
sortIndexes sort terms =
  let sorted = List.sortBy (sortTerms sort) terms
      indexes = List.map (termIndex sorted) terms
  in indexes

--------------------------------------------------------------

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
