module EnemySmash.Main exposing (main)

import Html.App as Html
import EnemySmash.Model exposing (Model, model)
import EnemySmash.View exposing (view)
import EnemySmash.Update exposing (Msg, update, subscriptions)


main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : ( Model, Cmd Msg )
init =
  model ! []
