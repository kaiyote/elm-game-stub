module Rogueish.Update exposing (update, subscriptions)

import Rogueish.Model exposing (Model, Msg(..))
import Rogueish.MapGen as RMG
import Random as R
import String as S
import Task as T


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NewSeed f ->
      { model | seed = R.initialSeed f } ! [ T.perform (\_ -> Cave) (\_ -> Cave) <| T.succeed 0 ]

    Cave ->
      let
        (level, seed') =
          RMG.randomCave (50, 35) model.seed
      in
        { model
        | level = level
        , seed = seed'
        } ! []
