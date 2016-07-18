module Rogueish.MapGen exposing (..)

import Array as A
import Matrix as M
import Matrix.Extra as ME
import Rogueish.Model exposing (..)
import Random as R exposing (Generator, Seed)


getNeighbors : Coord -> Grid Tile -> List Tile
getNeighbors (x, y) grid =
  let
    neighbors =
      ME.neighbours x y grid

    point =
      Maybe.withDefault Wall <| M.get x y grid
  in
    case List.length neighbors of
      8 ->
        point :: neighbors

      _ ->
        point :: (neighbors ++ List.repeat (8 - (List.length neighbors)) Wall)


numberOfWalls : Coord -> Grid Tile -> Int
numberOfWalls coord grid =
  getNeighbors coord grid
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
