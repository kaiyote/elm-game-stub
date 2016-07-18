module Rogueish.Model exposing (..)

import Matrix as M
import Random as R exposing (Seed)
import Task
import Time as T


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


type Msg
  = NoOp
  | NewSeed Int
  | Iterate
  | Iterate2
  | Generate
  | Cave


model : Model
model =
  Model M.empty (R.initialSeed 0) 0 0 ""


init : ( Model, Cmd Msg )
init =
  model ! [ Task.perform (\_ -> NewSeed 0) (\t -> NewSeed <| round t) T.now ]
