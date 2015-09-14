import Html exposing (Html)
import Signal exposing (Signal, Address)
import Effects exposing (Never)

import Animation.ToggleExample

import Topics.TopicList exposing (init, update, view, Model, Action)
--import Square.SpinSquare exposing (init, update, view, Model, Action)
import StartApp exposing (App, Config)
import Task

app = StartApp.start config

config : Config Model Action
config =
  { init = init
  , update = update
  , view = view
  , inputs = []
  }

main : Signal Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
