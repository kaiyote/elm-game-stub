module Rogueish.Model exposing (..)

import Matrix as M


type alias Coord = (Int, Int)


type alias Size = (Int, Int)


type alias Grid a = M.Matrix a


type alias Model =
  { level : Grid Tile
  }


type Tile
  = Floor
  | Wall


model : Model
model =
  Model M.empty
