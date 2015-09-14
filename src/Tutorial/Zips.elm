module Tutorial.Zips where

import Debug
import Http

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing ((:=))
import Task exposing (..)
import String

-- VIEW -----------------------------------------------------------

view : String -> Result String (List String) -> Html
view string result =
  let field =
        input
          [ placeholder "Zip Code"
          , value string
          , on "input" targetValue (Signal.message query.address)
          , myStyle
          ]
          []

      messages =
        case result of
          Err msg ->
              [ div [ myStyle ] [ text msg ] ]

          Ok cities ->
              List.map (\city -> div [ myStyle ] [ text city ]) cities
  in
      div [] (field :: messages)


--------------------------------------------------------------------

placesMailbox : Signal.Mailbox (List String)
placesMailbox = Signal.mailbox []

lookupZipCode : String -> Task String (List String)
lookupZipCode query =
  let url = ("http://api.zippopotam.us/us/" ++ query) in
  Http.get places url
    |> mapError (always "not found")

places : Json.Decoder (List String)
places =
  let place =
        Json.object2 (\city state -> city ++ ", " ++ state)
          ("place name" := Json.string)
          ("state" := Json.string)
  in
      "places" := Json.list place

query : Signal.Mailbox String
query =
  Signal.mailbox ""

results : Signal.Mailbox (Result String (List String))
results =
  Signal.mailbox (Err "A Valid US zip code is 5 numbers")

port requests : Signal (Task x ())
port requests =
  Signal.map lookupZipCode query.signal
    |> Signal.map (\task -> Task.toResult task `andThen` Signal.send results.address)

main = Signal.map2 view query.signal results.signal

myStyle : Attribute
myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]

-- you can't OWN the effects
-- need some way of combinging them with other ones though?
-- and if it's just

-- port runZip : Task x ()
-- port runZip =
  -- lookupZipCode "84108"
    -- `andThen` \res -> report res

-- report : Result Http.Error (List String) -> Task x ()
-- report res =
  -- let d = Debug.log "res" (res) in
  -- case res of
    -- Err _ -> Signal.send placesMailbox.address ["nothing"]
    -- Ok places ->
      -- Signal.send placesMailbox.address places
