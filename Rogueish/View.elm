module Rogueish.View exposing (view)

import Html exposing (..)
import Rogueish.Model exposing (Model)


view : Model -> Html msg
view model =
  text "rogueish"
