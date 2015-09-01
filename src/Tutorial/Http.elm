module Tutorial.Http where

import Debug
import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (..)

import Time exposing (second, Time)
import Graphics.Element exposing (show, Element)
-- Mailboxes: Mailbox Address Signal
-- send: Address a -> a -> Task x ()
-- as soon as the Task is performed, it shows up in the signal

main : Signal Element
main = Signal.map show placesMailbox.signal

contentMailbox : Signal.Mailbox String
contentMailbox = Signal.mailbox ""

port updateContent : Task x ()
port updateContent =
  Signal.send contentMailbox.address "hello!"


--------------------------------------------------------------------

placesMailbox : Signal.Mailbox (List String)
placesMailbox = Signal.mailbox []

lookupZipCode : String -> Task y (Result Http.Error (List String))
lookupZipCode query =
    Task.toResult <| Http.get places ("http://api.zippopotam.us/us/" ++ query)

places : Json.Decoder (List String)
places =
  let place =
        Json.object2 (\city state -> city ++ ", " ++ state)
          ("place name" := Json.string)
          ("state" := Json.string)
  in
      "places" := Json.list place

port runZip : Task x ()
port runZip =
  lookupZipCode "84108"
    `andThen` \res -> report res

report : Result Http.Error (List String) -> Task x ()
report res =
  let d = Debug.log "res" (res) in
  case res of
    Err _ -> Signal.send placesMailbox.address ["nothing"]
    Ok places ->
      Signal.send placesMailbox.address places
