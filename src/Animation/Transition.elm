module Animation.Transition where

import Animation exposing (from, to, duration, animation, Animation)
import Time exposing (Time, second)

type alias Transition = List Animation

none : Transition
none = []

add : Animation -> List Animation -> List Animation
add a trans = a :: trans

animate : Time -> List Animation -> Float
animate time anims =
  List.sum <| List.map (Animation.animate time) anims

dump : Time -> List Animation -> String
dump time trans = toString <| List.map (Animation.animate time) trans

-- additive animations always start at a value and go to zero
-- you can use additive time |> from N if you don't want to do the math later
additive : Time -> Float -> Float -> Animation
additive t old new =
  let diffFromNew = old - new in
  animation t |> from diffFromNew |> to 0
