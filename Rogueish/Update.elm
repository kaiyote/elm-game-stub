module Rogueish.Update exposing (update, subscriptions)

import Rogueish.Model exposing (Model, Msg(..))
import Rogueish.MapGen3 as RMG
import Random as R
import String as S
import Time as T
import Task


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NewSeed f ->
      { model | seed = R.initialSeed f } ! []

    Cave ->
      let
        (level', seed') =
          RMG.generate 10 (50, 35) model.seed
      in
        { model | level = level', seed = seed' } ! []
