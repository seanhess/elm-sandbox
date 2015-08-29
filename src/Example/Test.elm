module Example.Test where
import Html exposing (div, button, text, Html, h1, li, a, input, form, Attribute)
import Html.Attributes exposing (style, value, action, key)
import Html.Events exposing (onClick, on, targetValue, onSubmit)
import Signal exposing (Signal, Address)
import StartApp.Simple as StartApp
--import Style exposing (clickable)
import Debug
import Svg exposing (svg, rect, circle, g)
import Svg.Attributes exposing (width, height, viewBox)

import Svg exposing (svg, rect, g, text')
--import Svg.Events as SE
import Svg.Attributes as SA exposing (x, y, width, height, rx, ry, fill, textAnchor, transform)
import Example.Card as Card exposing (Card, Action(..))

main : Signal Html
main =
  StartApp.start { model = emptyModel, view = view, update = update }

type ItemSort = SortAlpha | SortId

type alias Model =
  { cards : List Card
  , uid : Int
  , sort : ItemSort
  , editing : Maybe Card
  , newText : String
  }

emptyModel : Model
emptyModel = { cards = [Card 1 "one", Card 2 "two", Card 3 "three"], editing = Nothing, sort = SortId, uid = 4, newText = "" }

newCard : Int -> String -> Card
newCard uid n = { id = uid, name = n }

type Action
    = NoOp
    | Add
    | Edit Int
    | DeleteInline Int
    | ChangeSort ItemSort
    | OpenedCard Card.Action
    | UpdateNewText String


deleteCard : Int -> List Card -> List Card
deleteCard id cards = List.filter (not << isId id) cards

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
        model

    Add ->
      case model.newText of
        "" -> model
        txt ->
          Debug.watch "add"
          { model | cards <- model.cards ++ [newCard model.uid model.newText]
                  , uid <- model.uid + 1
                  , newText <- ""
          }

    (ChangeSort srt) ->
      { model | sort <- srt }

    (Edit id) ->
      { model | editing <-
          List.head (List.filter (isId id) model.cards)
      }

    (UpdateNewText txt) ->
      { model | newText <- txt }

    (OpenedCard Delete) ->
      case model.editing of
        Nothing -> model
        Just card ->
          let cards = List.filter (not << isId card.id) model.cards
          in
          { model | editing <- Nothing
                  , cards <- cards
          }

    (DeleteInline id) ->
      { model | cards <- deleteCard id model.cards }

    (OpenedCard Save) ->
      case model.editing of
        Nothing -> model
        Just card ->
          -- replace it in the list
          -- set editing to nothing
          let cards = List.map (replaceWith (isId card.id) (\_ -> card)) model.cards
          in
          { model | editing <- Nothing
                  , cards <- cards
          }

    (OpenedCard act) ->
      case model.editing of
        Nothing -> model
        Just card ->
          { model | editing <- Just (Card.update act card)}



view : Address Action -> Model -> Html
view address model =
  let content =
    case model.editing of
      Nothing ->
        listPage address model

      Just card ->
        Card.view (Signal.forwardTo address OpenedCard) card
  in
  div [ ]
    [ h1 []  [ text "Apocalyptica" ]
    , content
    ]

listPage : Address Action -> Model -> Html
listPage address model =
  div [ style [("background-color", "red")] ]
    [ form [ onSubmit address Add, action "javascript:none" ]
        [ button [ ] [ text "Addz" ]
        , input
            [ value model.newText
            , on "input" targetValue (Signal.message address << UpdateNewText)
            ]
            []
        ]
    , div []
        [ button [ onClick address (ChangeSort SortAlpha)] [ text "sort A-Z" ]
        , button [ onClick address (ChangeSort SortId)] [ text "sort ID" ]
        ]
    , div [] [ cardList' address model ]
    ]


---------------------------------------------

isId : Int -> Card -> Bool
isId id card = card.id == id

replaceWith : (a -> Bool) -> (a -> a) -> a -> a
replaceWith p trans item =
  if p item
    then trans item
    else item

sortCards : ItemSort -> List Card -> List Card
sortCards s cs =
  case s of
    SortId -> List.sortBy .id cs
    SortAlpha -> List.sortBy .name cs

----------------------------------------------------

cardList : Address Action -> Model -> Html
cardList address model =
  svg
    [ width "500", height "500", viewBox "0 0 500 500" ]
    (List.indexedMap (cardItem address) (sortCards model.sort model.cards))


cardItem : Address Action -> Int -> Card -> Html
cardItem address index card =
  let x' = 100
      y' = 50 + (index * 100)
      tm = "translate(" ++ toString x' ++ "," ++ toString y' ++ ")"
  in
  g
    [ key (toString card.id)
    , width "100"
    , height "100"
    , transform tm
    ]
    [ rect
        [ width "100"
        , height "100"
        , x "0"
        , y "0"
        , rx "15"
        , ry "15"
        , SA.style "fill: #0000FF"
        ] []
    , text' [ fill "black", textAnchor "middle"] [ text card.name ]
    ]

cardList' : Address Action -> Model -> Html
cardList' address model =
  div
    [ style [("background","green"), ("width","400"), ("height", "400")] ]
    (List.indexedMap (cardItem' address) (sortCards model.sort model.cards))

cardItem' : Address Action -> Int -> Card -> Html
cardItem' address index card =
  let x' = 100
      y' = 50 + (index * 40)
      tm = "translate(" ++ toString x' ++ "px," ++ toString y' ++ "px) rotate(20deg)"
      k = Debug.watch "key" (toString card.id)
  in
  div
    [ key k
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

