module Tutorial.Mailboxes where

import TaskTutorial exposing (print, getCurrentTime)
import Time exposing (second, Time)
import Task exposing (Task, andThen)
import Graphics.Element exposing (show, Element)
-- Mailboxes: Mailbox Address Signal
-- send: Address a -> a -> Task x ()
-- as soon as the Task is performed, it shows up in the signal

main : Signal Element
main = Signal.map show contentMailbox.signal

contentMailbox : Signal.Mailbox String
contentMailbox = Signal.mailbox ""

port updateContent : Task x ()
port updateContent =
  Signal.send contentMailbox.address "hello!"
