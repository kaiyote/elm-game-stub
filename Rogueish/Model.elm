module Rogueish.Model exposing (..)

import Random as R exposing (Seed)
import Matrix as M exposing (Matrix)
import Time as T
import Set exposing (Set)
import Task


type alias Coord = (Int, Int)


type alias Size = (Int, Int)


type alias RoomID = Int


type alias Grid a = Matrix a


type alias Room =
  { origin : Coord
  , connections : Set RoomID
  , size : Size
  }


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
  | Empty


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
