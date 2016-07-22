module Rogueish.View exposing (view)

import Html exposing (..)
import Html.Events exposing (..)
import Rogueish.Model exposing (Model, Grid, Tile(..), Msg(..))
import Matrix as M
import Matrix.Extra as ME


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Cave ] [ text "Create Level"]
    , M.map convertTileToChar model.level |> ME.prettyPrint
    ]


convertTileToChar : Tile -> Char
convertTileToChar tile =
  case tile of
    Wall -> '█'
    Floor -> '.'
    Hero -> '@'
    Enemy -> 's'
    Empty -> ' '
