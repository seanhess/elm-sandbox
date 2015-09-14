module Animation.ListExample where

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
import Debug

--------------------------------------------------------------------

type alias Transition =
  { animations : List Animation
  , time : Time
  }

none = { animations = [], time = 0 }

add : Animation -> Transition -> Transition
add a trans = { trans | animations <- a :: trans.animations }

tick : Time -> Transition -> Transition
tick time trans =
  let anims = List.filter (not << Animation.isDone time) trans.animations
  in { trans | animations <- anims
             , time <- time
     }

value : Transition -> Float
value trans =
  List.sum <| List.map (animate trans.time) trans.animations

dump : Transition -> String
dump trans =
  String.join "\n"
    [ "time: " ++ toString (round (trans.time / 1000))
    , "num: " ++ toString (List.length trans.animations)
    , toString <| List.map (animate trans.time) trans.animations
    ]

anim : Time -> Float -> Animation
anim t s =
  animation t |> from s |> to 0 |> duration (0.8 * second)

--------------------------------------------------------------------

type PageState = Open | Closed
type SortOrder = SortName | SortId

type alias Item =
  { id : Int
  , name : String
  }

type alias Model =
  { sort : SortOrder
  , items : List ItemModel
  }

init : ( Model, Effects Action )
init =
  ( { sort = SortId
    , items = List.indexedMap itemModel [Item 1 "one", Item 2 "two", Item 3 "three", Item 4 "four", Item 5 "five", Item 6 "six"]
    }
  , Effects.tick Tick )

-----------------------------------------------------------------------

type Action
  = Sort SortOrder
  | Tick Time

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Sort order ->
      -- for each one:
      -- get the new position
      let items = List.indexedMap updatePosition <| sortItems order model.items
      in
      ( { model | sort <- order, items <- items }
      , Effects.none )

    Tick t ->
      let items = List.map (updateItem (ItemTick t)) model.items in
      ( { model | items <- items }
      , Effects.tick Tick )

-----------------------------------------------------------------------------

sortItems : SortOrder -> List ItemModel -> List ItemModel
sortItems order items =
  case order of
    SortName -> List.sortBy (.name << .item) items
    SortId -> List.sortBy (.id << .item) items

view : Address Action -> Model -> Html
view address model =
  div [ style [("margin", "10px")] ]
    [ div [ style [("margin-bottom", "10px")] ]
        [ button
            [ style (buttonStyle (model.sort == SortName))
            , onClick address (Sort SortName) ]
            [ text "Sort Name" ]
        , button
            [ style (buttonStyle (model.sort == SortId))
            , onClick address (Sort SortId) ]
            [ text "Sort Id" ]
        ]
    -- , div
        -- []
        -- (List.map (\model -> pre [] [ text (dump model.transition) ]) model.items)
    , div
        [ style [("position", "relative")] ]
        (List.map (itemView address) model.items)
    ]


-------------------------------------------------------

-- you can't generate these on the fly, you have to save / store them
type alias ItemModel =
  { item : Item
  , transition : Transition
  , position : Int
  }

itemModel : Int -> Item -> ItemModel
itemModel pos item = { item = item, position = pos, transition = none }

updatePosition : Int -> ItemModel -> ItemModel
updatePosition pos model =
  let dy = itemY model.position - itemY pos in
  { model
    | position <- pos
    , transition <- add (anim model.transition.time dy) model.transition
  }

type ItemAction = ItemTick Time

updateItem : ItemAction -> ItemModel -> ItemModel
updateItem action model =
  case action of
    ItemTick time ->
      { model | transition <- tick time model.transition }

itemY : Int -> Float
itemY pos = toFloat pos * 50.0

itemView : Address Action -> ItemModel -> Html
itemView address model =
  let y = itemY model.position + value model.transition in
  div
    [ style
        [ ("background", "#E6E6EF")
        , ("border", "solid 1px #716DCF")
        , ("borderRightWidth", "10px")
        , ("borderLeftWidth", "0px")
        , ("borderTopWidth", "0px")
        , ("margin", "10px")
        , ("marginLeft", "0px")
        , ("padding", "10px")
        , ("width", "320px")
        , ("position", "absolute")
        , ("top", toString y ++ "px")
        ]
    ]
    [ text model.item.name ]

maxWidth = 200.0


buttonStyle isSelected =
  let highlight =
    if isSelected
    then
      [ ("borderBottomWidth", "4px")
      , ("background", "#FFF6D6")
      ]
    else []
  in
  [ ("color", "#555")
  , ("text-transform", "uppercase")
  , ("cursor", "pointer")
  , ("letter-spacing", "0.15em")
  , ("padding", "8px")
  , ("border", "solid 2px #555")
  , ("background", "white")
  , ("borderRadius", "6px")
  , ("fontWeight", "bold")
  , ("font", "11px 'HelveticaNeue'")
  , ("outline", "none")
  , ("marginRight", "10px")
  ] ++ highlight

------------------------------------------------------------------------------

app =
  StartApp.start { init = init, view = view, update = update, inputs = [] }

main : Signal Html
main = app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks

