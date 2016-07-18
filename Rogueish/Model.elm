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
  , hero : Creature
  , enemies : List Creature
  }


type alias Creature =
  { health : Int
  , damage : Int
  , position : Coord
  }


type Tile
  = Floor
  | Wall
  | Hero
  | Enemy


type Msg
  = NewSeed Int
  | Cave


model : Model
model =
  Model M.empty (R.initialSeed 0) hero []


init : ( Model, Cmd Msg )
init =
  model ! [ Task.perform (\_ -> NewSeed 0) (\t -> NewSeed <| round t) T.now ]


hero : Creature
hero =
  Creature 100 10 (0, 0)


enemy : Creature
enemy =
  Creature 10 5 (0, 0)
