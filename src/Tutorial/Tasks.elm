module Tutorial.Tasks where

import TaskTutorial exposing (print, getCurrentTime)
import Time exposing (second, Time)
import Task exposing (Task, andThen)
import Graphics.Element exposing (show)


-- A signal that updates to the current time every second
clock : Signal Time
clock =
  Time.every second


-- Turn the clock into a signal of tasks
printTasks : Signal (Task x ())
printTasks =
  Signal.map print clock

-- Actually perform all those tasks
-- port runner : Signal (Task x ())
-- port runner =
  -- printTasks


-- TASKS

port runner : Task x ()
port runner = getCurrentTime `andThen` print

