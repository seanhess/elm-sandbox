--import TaskTutorial exposing (getCurrentTime, print)
import Task exposing (Task, andThen)
import Signal
import Graphics.Element exposing (show, Element)

--port runner : Task x ()
--port runner =
  --getCurrentTime `andThen` print

main : Signal Element
main =
  Signal.map show contentMailbox.signal

contentMailbox : Signal.Mailbox String
contentMailbox =
  Signal.mailbox ""

port updateContent : Task x ()
port updateContent =
  Signal.send contentMailbox.address "hello!"
