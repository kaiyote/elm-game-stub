{- My attempt at an Elm port of: https://github.com/jamiltron/Thieflike/blob/post03/src/LevelGen.hs-}
module Rogueish.MapGen2 exposing (..)

import Random as R exposing (Seed)
import Set as S exposing (Set)


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


genLevel : Int -> Int -> Int -> Int -> Seed -> String
genLevel rows cols maxWidth maxHeight seed =
  let
    colWidth =
      maxWidth // cols

    rowHeight =
      maxHeight // rows
  in
    List.concatMap (\a -> List.map (\b -> (a, b)) [0..cols - 1]) [0..rows - 1]
      |> List.scanl (\gridid (room, seed') -> genRoom rows cols colWidth rowHeight seed' gridid)
      |> digRooms maxWidth maxHeight


genRoom : Int -> Int -> Int -> Int -> Seed -> GridID -> (Room, Seed)
genRoom rows cols colWidth rowHeight seed (c, r) =
  let
    (minX, minY) =
      (c * colWidth, r * rowHeight)

    (xStart, seed') =
      R.step (R.int (minX + 1) (minX + coldWidth - 7)) seed

    (yStart, seed'') =
      R.step (R.int ) seed'

    (xEnd, seed''') =
      R.step (R.int ) seed''

    (yEnd, seed'''') =
      R.step (R.int ) seed'''
  in
    Room S.empty ((xStart, yStart), (xEnd, yEnd)) (c, r)
      |> genConns rows cols seed''''
