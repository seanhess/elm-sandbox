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
import List.Extra as List
import String
import Debug
import Animation.Transition as Trans exposing (Transition, add, additive)

type PageState = Open | Closed
type SortOrder = SortName | SortId

type alias Item =
  { id : Int
  , name : String
  }

type alias Model =
  { sort : SortOrder
  , items : List ItemModel
  , time : Time
  }

init : ( Model, Effects Action )
init =
  ( { sort = SortId
    , items = List.map itemModel [Item 1 "one", Item 2 "two", Item 3 "three", Item 4 "four", Item 5 "five", Item 6 "six"]
    , time = 0
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
      let oldSort = sortItems model.sort model.items
          newSort = sortItems order model.items
          items =
            -- f : Int -> ItemModel -> (Int -> ItemModel -> ItemModel)
            -- List.map2 
            -- List.indexedMap (updatePosition model.time) newSort
            -- List.map2 (,) 
      in
      -- ok now I need to zip them all together!
      ( { model | sort <- order }
      , Effects.none )

    Tick t ->
      ( { model | time <- t }
      , Effects.tick Tick )

-- indexedMap = (Int -> a -> b)
-- map2 (a -> b -> result)

updatePosition : Time -> Int -> Int -> ItemModel -> ItemModel
updatePosition time old pos model =
  { model | transition <- add (additive time (toFloat old) (toFloat pos)) model.transition }

-----------------------------------------------------------------------------

sortItems : SortOrder -> List ItemModel -> List ItemModel
sortItems order items =
  case order of
    SortName -> List.sortBy (.item >> .name) items
    SortId -> List.sortBy (.item >> .id) items

view : Address Action -> Model -> Html
view address model =
  let sortedItems = sortItems model.sort model.items
  in
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
        (List.indexedMap (itemView address model.time) sortedItems)
    ]


-------------------------------------------------------

-- you can't generate these on the fly, you have to save / store them
type alias ItemModel =
  { item : Item
  , transition : Transition
  }

itemModel : Item -> ItemModel
itemModel item = { item = item, transition = Trans.none }

type ItemAction = ItemTick Time

itemY : Float -> Float
itemY pos = pos * 50.0


itemView : Address Action -> Time -> Int -> ItemModel -> Html
itemView address time pos model =
  let y = itemY (toFloat pos + Trans.animate time model.transition) in
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
