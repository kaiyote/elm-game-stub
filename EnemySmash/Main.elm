module EnemySmash.Main exposing (main)

import Html.App as Html
import EnemySmash.Model as ESModel
import EnemySmash.View as ESView
import EnemySmash.Update as ESUpdate


main : Program Never
main =
  Html.program
    { init = init
    , view = ESView.view
    , update = ESUpdate.update
    , subscriptions = ESUpdate.subscriptions
    }


init : ( ESModel.Model, Cmd ESUpdate.Msg )
init =
  ESModel.model ! []
