module Main exposing (..)

import View exposing (view)
import Update exposing (Msg(..), update)
import Model exposing (Model, init, size)
import Keyboard exposing (ups, downs)
import AnimationFrame exposing (diffs)
import Time exposing (every, second)
import Html.App as Html


main : Program Never
main =
  Html.program
    { init = init
    , view = view size
    , update = update
    , subscriptions = subscriptions
    }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ diffs Update
    , every second Tick
    , downs KeyDown
    , ups KeyUp
    ]
