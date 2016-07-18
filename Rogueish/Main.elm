module Rogueish.Main exposing (main)

import Html.App as Html
import Rogueish.Model exposing (init)
import Rogueish.View exposing (view)
import Rogueish.Update exposing (update, subscriptions)


main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



