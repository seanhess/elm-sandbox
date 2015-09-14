module Animation.ListExample where

import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import StartApp as StartApp
import Signal exposing (Address)
import Animation exposing (animation, from, to, duration, delay, Animation, static, animate)
import Time exposing (Time, second)
import Effects exposing (Effects, Never)
import Task exposing (Task)
import List
import String
import Debug

type PageState = Open | Closed
type SortOrder = SortName | SortId

type alias Item =
  { id : Int
  , name : String
  }

type alias Model =
  { sort : SortOrder
  , items : List ItemModel
  , newItemText : String
  }

init : ( Model, Effects Action )
init =
  ( { sort = SortId
    , items = List.map itemModel [Item 1 "one", Item 2 "two", Item 3 "three", Item 4 "four", Item 5 "five", Item 6 "six"]
    , newItemText = ""
    }
  , Effects.tick Tick )

-----------------------------------------------------------------------

type Action
  = Sort SortOrder
  | Tick Time
  | UpdateText String
  | NewItem

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Sort order ->
      -- TODO calculate old sort position (old order)
      -- TODO calculate new sort position (new order)
      -- TODO calculate dy for each one

      ( { model | sort <- order }
      , Effects.none )

    UpdateText txt ->
      ( { model | newItemText <- txt }
      , Effects.none )

    -- can't do this yet because I'm storing the list order
    -- no good. Everything has to be transitive
    NewItem ->
      -- let item = Item (nextId model.items) model.newItemText in
      ( { model | newItemText <- "" }
      , Effects.none )

    Tick t ->
      let items = List.map (updateTime t) model.items in
      ( { model | items <- items }
      , Effects.tick Tick )

nextId : List ItemModel -> Int
nextId mds = List.length mds + 1

-----------------------------------------------------------------------------

sortItems : SortOrder -> List ItemModel -> List ItemModel
sortItems order items =
  case order of
    SortName -> List.sortBy (.name << .item) items
    SortId -> List.sortBy (.id << .item) items

view : Address Action -> Model -> Html
view address model =
  let sortedItems = sortItems model.sort model.items in
  div [ style [("margin", "10px")] ]
    [ p [] [ itemInput address model ]
    , div [ style [("margin-bottom", "10px")] ]
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
        (List.indexedMap (itemView address) sortedItems)
    ]

itemInput : Address Action -> Model -> Html
itemInput address model =
  H.form
    [ style [("marginBottom", "15px")]
    , action "javascript:void(0)"
    , onSubmit address NewItem
    ]
    [ input
        [ type' "text"
        , placeholder "search"
        , A.value model.newItemText
        , on "input" targetValue (Signal.message address << UpdateText)
        , style [("fontSize", "16px"), ("padding", "4px"), ("width", "200px"), ("marginBottom", "6px")]
        ] []
    ]



-------------------------------------------------------

-- you can't generate these on the fly, you have to save / store them
type alias ItemModel =
  { item : Item
  , transition : Transition
  }

itemModel : Item -> ItemModel
itemModel item = { item = item, transition = none }

updateTime : Time -> ItemModel -> ItemModel
updateTime time model =
  { model | transition <- tick time model.transition }

itemY : Int -> Float
itemY pos = toFloat pos * 50.0

itemView : Address Action -> Int -> ItemModel -> Html
itemView address pos model =
  let y = itemY pos + value model.transition in
  div
    [ style (itemStyle y) ]
    [ text model.item.name ]


maxWidth = 200.0

-------------------------------------------------

type alias Style = List (String, String)

inputStyle : Style
inputStyle =
  [ ("fontSize", "18px")
  , ("padding", "4px" )
  ]

itemStyle : Float -> Style
itemStyle y =
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




--------------------------------------------------------------------
-- transition library

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
