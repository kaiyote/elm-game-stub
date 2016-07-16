module Rogueish.Update exposing (Msg(..), update, subscriptions)

import Rogueish.Model exposing (Model)
import Rogueish.MapGen as RMG
import Random as R
import String as S


type Msg
  = NoOp
  | NewSeed Int
  | Iterate
  | Iterate2
  | Generate
  | XUpdate String
  | YUpdate String
  | NumWalls


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
          RMG.randomMap (30, 30) model.seed
      in
        { model
        | level = level
        , seed = seed'
        } ! []

    Iterate ->
      { model | level = RMG.iterate model.level } ! []

    Iterate2 ->
      { model | level = RMG.iterate2 model.level } ! []

    XUpdate x ->
      { model | x = Result.withDefault 0 <| S.toInt x } ! []

    YUpdate y ->
      { model | y = Result.withDefault 0 <| S.toInt y } ! []

    NumWalls ->
      { model | numWalls = toString <| RMG.numberOfWalls (model.x, model.y) model.level } ! []
