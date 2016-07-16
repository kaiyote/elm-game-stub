module Rogueish.View exposing (view)

import Html exposing (..)
import Html.Events exposing (..)
import Rogueish.Model exposing (Model, Grid, Tile(..))
import Rogueish.Update exposing (Msg(..))
import Matrix as M
import Matrix.Extra as ME


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Generate ] [ text "Generate" ]
    , button [ onClick Iterate ] [ text "Iterate" ]
    , button [ onClick Iterate2 ] [ text "Iterate2" ]
    , input [ onInput XUpdate ] []
    , input [ onInput YUpdate ] []
    , button [ onClick NumWalls ] [ text "Number of Walls" ]
    , text model.numWalls
    , M.map convertTileToChar model.level |> ME.prettyPrint
    ]


convertTileToChar : Tile -> String
convertTileToChar tile =
  case tile of
    Wall -> "█"
    Floor -> "."
