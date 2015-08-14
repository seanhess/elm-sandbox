import Html exposing (div, button, text, Html, h1, li, a, input)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, on, targetValue)
import Signal exposing (Signal, Address)
import StartApp
import Random exposing (Seed)
import Card exposing (Card, Action(..))
import Style exposing (clickable)

main =
  StartApp.start { model = emptyModel, view = view, update = update }

type alias Model =
  { cards : List Card
  , uid : Int
  , editing : Maybe Card
  }

emptyModel : Model
emptyModel = { cards = [], editing = Nothing, uid = 1 }

newCard : Int -> String -> Card
newCard uid n = { id = uid, name = n }

type Action
    = NoOp
    | Add
    | Edit Int
    | OpenedCard Card.Action

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
        model

    Add ->
      { model | cards <- model.cards ++ [newCard model.uid "New Card"]
              , uid <- model.uid + 1
      }

    (Edit id) ->
      { model | editing <-
          List.head (List.filter (isId id) model.cards)
      }

    (OpenedCard Delete) ->
      case model.editing of
        Nothing -> model
        Just card ->
          let cards = List.filter (not << isId card.id) model.cards
          in
          { model | editing <- Nothing
                  , cards <- cards
          }

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
    [ button [ onClick address Add ] [ text "Add" ]
    , div [] (List.map (cardItem address) model.cards)
    ]

cardItem : Address Action -> Card -> Html
cardItem address card =
  li []
    [ a
        [ style clickable
        , onClick address (Edit card.id)
        ]
        [ text card.name ]
    ]

---------------------------------------------

isId : Int -> Card -> Bool
isId id card = card.id == id

replaceWith : (a -> Bool) -> (a -> a) -> a -> a
replaceWith p trans item =
  if p item
    then trans item
    else item
