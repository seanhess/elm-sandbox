module Example.StateProps where
import Html exposing (div, button, text, Html, h1, hr, h4, ul, li)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp
import Signal exposing (Address)

import StateProps.WordSelect as Select

type alias Model =
  { select : Select.State
  , words : List String
  }

init : Model
init = { select = Select.init, words = [] }



------------------------------------------------

type Action
  = Select Select.Action

update : Action -> Model -> Model
update action model =
  case action of
    Select act ->

      let (select, event) = Select.update act model.select

          words = case event of
            Select.Add word ->
              word :: model.words

            Select.Delete word ->
              List.filter (\w -> w /= word) model.words

            _ -> model.words

      in { model | select <- select, words <- words }

main =
  StartApp.start { model = init, view = view, update = update }

view : Address Action -> Model -> Html
view address model =
  div [ style [("margin", "10px")] ]
    [ h1 [] [ text "List of Words" ]
    , Select.view (Signal.forwardTo address Select) { words = model.words } model.select
    , hr [] []
    , h4 [] [ text "Words used again in a different view:"]
    , ul [] (List.map wordView model.words)
    ]

wordView : String -> Html
wordView word = li [] [ text word ]



