module Card where

import Html exposing (div, button, text, Html, h1, li, a, input)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, on, targetValue)
import Signal exposing (Signal, Address)

type alias Card =
  { id : Int
  , name : String
  }

type Action
  = UpdateName String
  | Delete
  | Save

update : Action -> Card -> Card
update action card =
  case action of
    UpdateName nm ->
      { card | name <- nm }

    _ -> card

view : Address Action -> Card -> Html
view address card =
  div
    [ style [("background-color", "blue")]]
    [ div []
      [ button
        [ onClick address Save ]
        [ text "Save" ]
      , button
        [ onClick address Delete ]
        [ text "Delete" ]
      ]
    , div []
      [ input
        [ value card.name
        , on "input" targetValue (Signal.message address << UpdateName)
        ]
        []
      ]
    ]
