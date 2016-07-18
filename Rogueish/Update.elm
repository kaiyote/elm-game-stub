module Rogueish.Update exposing (update, subscriptions)

import Rogueish.Model exposing (Model, Msg(..))
import Rogueish.MapGen as RMG
import Random as R
import String as S


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case Debug.log "msg" msg of
    NoOp ->
      model ! []

    NewSeed f ->
      { model | seed = R.initialSeed f } ! []

    Generate ->
      let
        (level, seed') =
          RMG.randomMap (50, 35) model.seed
      in
        { model
        | level = level
        , seed = seed'
        } ! []

    Iterate ->
      { model | level = RMG.iterate model.level } ! []

    Iterate2 ->
      { model | level = RMG.iterate2 model.level } ! []

    Cave ->
      let
        (level, seed') =
          RMG.randomCave (50, 35) model.seed
      in
        { model
        | level = level
        , seed = seed'
        } ! []
