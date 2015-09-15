module StateProps.WordSelect where

import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import Debug
import Effects exposing (Effects)
import Time exposing (Time)
import String

-- README: WordSelect owns the value of the input text box
-- but not the list of fully entered words.
-- Instead, that list of words is passed in from the parent

type alias State =
  -- the parent should own this, because it will be used everywhere else
  { currentText : String
  }

-- the parent should own this, because it will be used everywhere else
type alias Props =
  { words : List String
  }

init : State
init = { currentText = "" }

----------------------------

-- stuff that happens that edits state
type Action
  = Update String
  | Enter
  | OnDelete String

-- stuff that happens that we report to the parent. there are multiple ways to do this. See elm-architecture-tutorial, example #4
-- https://github.com/evancz/elm-architecture-tutorial#example-4-a-fancier-list-of-counters
type Event
  = Add String
  | Delete String
  | None

-- I don't love this way of sending events up, but I can't think of a better way
update : Action -> State -> (State, Event)
update action state =
  case action of
    Update text ->
      ( { state | currentText <- text }
      , None )

    Enter ->
      ( { state | currentText <- "" }
      , Add state.currentText )

    OnDelete word ->
      ( state
      , Delete word )

view : Address Action -> Props -> State -> Html
view address props state =
  let tags = (List.map (wordView address) props.words) in
  H.form
    [ style [("marginBottom", "15px"), ("display", "flex"), ("flex-wrap", "wrap")]
    , action "javascript:none"
    , onSubmit address Enter
    ]
    ([ input
        [ type' "text"
        , placeholder "word"
        , value state.currentText
        , on "input" targetValue (Signal.message address << Update)
        , style [("fontSize", "16px"), ("padding", "4px"), ("width", "200px"), ("marginBottom", "6px")]
        ] []
    ] ++ tags)


wordView : Address Action -> String -> Html
wordView address word =
  span
    [ style
        [ ("border", "solid 1px #4CAE4C")
        , ("background", "#5CB85C")
        , ("color", "white")
        , ("display", "inline-block")
        , ("padding", "4px")
        , ("borderRadius", "2px")
        , ("paddingLeft", "12px")
        , ("paddingRight", "12px")
        , ("marginLeft", "4px")
        , ("marginBottom", "6px")
        ]
    ]
    [ span [ style [("marginRight", "10px")] ]
       [ a [ style clickable, onClick address (OnDelete word) ] [ text "×" ] ]
    , span [] [ text word ]
    ]


--------------------------

clickable =
  [ ("cursor", "pointer") ]
