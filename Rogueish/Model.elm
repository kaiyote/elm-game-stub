module Rogueish.Model exposing (..)

import Matrix as M
import Random as R exposing (Seed)


type alias Coord = (Int, Int)


type alias Size = (Int, Int)


type alias Grid a = M.Matrix a


type alias Model =
  { level : Grid Tile
  , seed : Seed
  , x : Int
  , y : Int
  , numWalls : String
  }


type Tile
  = Floor
  | Wall


model : Model
model =
  Model M.empty (R.initialSeed 0) 0 0 ""
