module Rogueish.MapGen exposing (..)

import Array as A
import Matrix as M
import Rogueish.Model exposing (..)
import Random as R exposing (Generator, Seed)


neighborhood : Coord -> List Coord
neighborhood (x, y) =
  [ (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
  , (x - 1, y), (x, y), (x + 1, y)
  , (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
  ]


neighborhood2 : Coord -> List Coord
neighborhood2 (x, y) =
  [ (x - 2, y - 2), (x - 1, y - 2), (x, y - 2), (x + 1, y - 2), (x + 2, y - 2)
  , (x - 2, y - 1), (x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x + 2, y - 1)
  , (x - 2, y), (x - 1, y), (x, y), (x + 1, y), (x + 2, y)
  , (x - 2, y + 1), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1), (x + 2, y + 1)
  , (x - 2, y + 2), (x - 1, y + 2), (x, y + 2), (x + 1, y + 2), (x + 2, y + 2)
  ]


getNeighborsOrDefault : a -> Coord -> Grid a -> List a
getNeighborsOrDefault default (x, y) grid =
  neighborhood (x, y)
    |> List.map (\(x', y') -> Maybe.withDefault default <| M.get x' y' grid)


getNeighborsOrDefault2 : a -> Coord -> Grid a -> List a
getNeighborsOrDefault2 default (x, y) grid =
  neighborhood2 (x, y)
    |> List.map (\(x', y') -> Maybe.withDefault default <| M.get x' y' grid)


getNeighbors : Coord -> Grid Tile -> List Tile
getNeighbors =
  getNeighborsOrDefault Wall


getNeighbors2 : Coord -> Grid Tile -> List Tile
getNeighbors2 =
  getNeighborsOrDefault2 Wall


numberOfWalls : Coord -> Grid Tile -> Int
numberOfWalls coord grid =
  getNeighbors coord grid
    |> List.filter (\t -> t == Wall)
    |> List.length


numberOfWalls2 : Coord -> Grid Tile -> Int
numberOfWalls2 coord grid =
  getNeighbors2 coord grid
    |> List.filter (\t -> t == Wall)
    |> List.length


randomTile : Generator Tile
randomTile =
  R.map (\f -> if f < 0.4 then Wall else Floor) (R.float 0 1)


randomMap : Size -> Seed -> ( Grid Tile, Seed )
randomMap (w, h) seed =
  let
    (map, seed') =
      R.list w randomTile
        |> R.list h
        |> flip R.step seed
  in
    (Maybe.withDefault M.empty <| M.fromList map, seed')


iterate : Grid Tile -> Grid Tile
iterate grid =
  M.toIndexedArray grid
    |> A.map fst
    |> A.toList
    |> List.map (\coord -> (coord, if (numberOfWalls coord grid) >= 5 then Wall else Floor))
    |> List.foldl (\((x, y), a) grid -> M.set x y a grid) grid


iterate2 : Grid Tile -> Grid Tile
iterate2 grid =
  M.toIndexedArray grid
    |> A.map fst
    |> A.toList
    |> List.map (\coord -> (coord, if numberOfWalls coord grid >= 5 then Wall else if numberOfWalls coord grid <= 2 then Wall else Floor))
    |> List.foldl (\((x, y), a) grid -> M.set x y a grid) grid


randomCave : Size -> Seed -> ( Grid Tile, Seed )
randomCave size seed =
  let
    (bedrock, seed') =
      randomMap size seed
  in
    ( bedrock |> iterate2 |> iterate |> iterate |> iterate |> iterate, seed')
