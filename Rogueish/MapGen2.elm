{- My attempt at an Elm port of: https://github.com/jamiltron/Thieflike/blob/post03/src/LevelGen.hs-}
module Rogueish.MapGen2 exposing (..)

import Random as R exposing (Seed)
import Set as S exposing (Set)
import Random.Array as RA
import Array as A
import List.Extra as LE


type alias GridID = (Int, Int)


type alias Coord = (Int, Int)


type alias Range = (Coord, Coord)


type alias Room =
  { connections : Set GridID
  , corners : Range
  , id : GridID
  }


minHeight = 6


minWidth = 6


genLevel : Int -> Int -> Int -> Int -> Seed -> List GridID
genLevel rows cols maxWidth maxHeight seed =
  let
    colWidth =
      maxWidth // cols

    rowHeight =
      maxHeight // rows
  in
    List.concatMap (\a -> List.map (\b -> (a, b)) [0..cols - 1]) [0..rows - 1]
      |> LE.scanl1 (\gridid (room, seed') -> genRoom rows cols colWidth rowHeight seed' gridid)
      --|> digRooms maxWidth maxHeight


genRoom : Int -> Int -> Int -> Int -> Seed -> GridID -> (Room, Seed)
genRoom rows cols colWidth rowHeight seed (c, r) =
  let
    (minX, minY) =
      (c * colWidth, r * rowHeight)

    (xStart, seed') =
      R.step (R.int (minX + 1) (minX + colWidth - 7)) seed

    (yStart, seed'') =
      R.step (R.int (minY + 1) (minY + rowHeight - 7)) seed'

    (xEnd, seed''') =
      R.step (R.int (xStart + minWidth) (minX + colWidth - 1)) seed''

    (yEnd, seed'''') =
      R.step (R.int (yStart + minHeight) (minY + rowHeight - 1)) seed'''
  in
    Room S.empty ((xStart, yStart), (xEnd, yEnd)) (c, r)
      |> genConns rows cols seed''''


genConns : Int -> Int -> Seed -> Room -> (Room, Seed)
genConns rows cols seed room =
  let
    adjs =
      adjRooms rows cols room.id

    (numConns, seed') =
      R.step (R.int 1 (List.length adjs)) seed

    (roomShuffle, seed'') =
      R.step (RA.shuffle (A.fromList adjs)) seed'
  in
    ({ room | connections = S.fromList <| List.take numConns <| A.toList roomShuffle }, seed'')


adjRooms : Int -> Int -> GridID -> List GridID
adjRooms rows cols (x, y) =
  let
    diffOne (x', y') =
      abs (x' - x) + abs (y' - y) == 1
  in
    neighbors rows cols (x, y)
      |> List.filter diffOne


neighbors : Int -> Int -> GridID -> List GridID
neighbors rows cols (x, y) =
  [ (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
  , (x - 1, y), (x + 1, y)
  , (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
  ] |> List.filter (\(x', y') -> x' >= 0 && x' < cols && y' >= 0 && y' < rows)
