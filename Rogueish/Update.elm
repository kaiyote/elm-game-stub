module Rogueish.Update exposing (Msg(..), update, subscriptions)

import Rogueish.Model exposing (Model)


type Msg
  = NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp -> model ! []
