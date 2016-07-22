{- My attempt at an Elm port of: https://github.com/jamiltron/Thieflike/blob/post03/src/LevelGen.hs-}
module Rogueish.MapGen2 exposing (..)

import Random as R exposing (Seed)
import Set as S exposing (Set)
import String
import Random.Array as RA
import Array as A
import Dict as D exposing (Dict)
import Matrix as M exposing (Matrix)
import Rogueish.Model exposing (..)
import List.Extra as LE


type alias GridID = (Int, Int)


type alias Coord = (Int, Int)


type alias Range = (Coord, Coord)


type alias CharMap = Dict Coord Char


type alias Room =
  { connections : Set GridID
  , corners : Range
  , id : GridID
  }


defaultRoom =
  Room S.empty ((0,0),(0,0)) (0,0)


minHeight = 6


minWidth = 6


genLevel : Int -> Int -> Int -> Int -> Seed -> Matrix Tile
genLevel rows cols maxWidth maxHeight seed =
  let
    colWidth =
      maxWidth // cols

    rowHeight =
      maxHeight // rows
  in
    genCoordSet 0 0 (cols - 1) (rows - 1)
      |> List.scanl (\gridid (_, seed') -> genRoom rows cols colWidth rowHeight seed' gridid) (defaultRoom, seed)
      |> List.map fst
      |> List.tail
      |> Maybe.withDefault []
      |> digRooms maxWidth maxHeight
      |> String.lines
      |> List.map String.toList
      |> LE.init
      |> Maybe.withDefault []
      |> M.fromList
      |> Maybe.withDefault M.empty
      |> M.map charToTile


charToTile : Char -> Tile
charToTile c =
  case c of
    '#' -> Wall
    '.' -> Floor
    _ -> Empty


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


digRooms : Int -> Int -> List Room -> String
digRooms maxW maxH rooms =
  let
    coords =
      genCoordSet 0 0 maxW maxH

    dug =
      List.map expandRoom rooms
        |> List.foldl (\roomMap fullMap -> D.union fullMap roomMap) D.empty
  in
    List.foldl (mapper dug maxH) "" coords
      |> String.reverse


mapper : CharMap -> Int -> Coord -> String -> String
mapper tmap maxW (x, y) s =
  if y == maxW then
    String.cons '\n' s
  else
    case D.get (x, y) tmap of
      Just c -> String.cons c s
      Nothing -> String.cons ' ' s


expandRoom : Room -> CharMap
expandRoom room =
  let
    ((x0, y0), (x1, y1)) =
      room.corners

    fill : Coord -> CharMap -> CharMap
    fill (x, y) lmap =
      if (x >= x0 && x <= x1 && y >= y0 && y <= y1) then
        D.insert (x, y) '.' lmap
      else
        D.insert (x, y) '#' lmap

  in
    genCoordSet (x0 - 1) (y0 - 1) (x1 + 1) (y1 + 1)
      |> List.foldl fill D.empty


genCoordSet : Int -> Int -> Int -> Int -> List Coord
genCoordSet minX minY maxX maxY =
  List.concatMap (\a -> List.map (\b -> (a, b)) [minY..maxY]) [minX..maxX]


connectCoords : Coord -> Coord -> CharMap -> CharMap
connectCoords (x, y) (w, z) cmap =
  let
    dx =
      abs (x - w)

    dy =
      abs (y - z)
  in
    if (x, y) == (w, z) then
      cmap
    else if dx > dy then
      if x < w then
        connectCoords (x+1, y) (w, z) <| hallInsert (x,y) cmap
      else
        connectCoords (x-1, y) (w, z) <| hallInsert (x,y) cmap
    else
      if y < z then
        connectCoords (x, y+1) (w, z) <| hallInsert (x, y) cmap
      else
        connectCoords (x, y-1) (w, z) <| hallInsert (x, y) cmap


hallInsert : Coord -> CharMap -> CharMap
hallInsert (x, y) m =
  let
    fillFunc : Char -> Maybe Char -> Maybe Char
    fillFunc newVal maybeThing =
      case maybeThing of
        Just c ->
          Just <| if c == ' ' then newVal else c
        Nothing ->
          Just newVal

    fill : Coord -> CharMap -> CharMap
    fill (x', y') m' =
      D.update (x', y') (fillFunc '#') m'
  in
    genCoordSet (x - 1) (y - 1) (x + 1) (y + 1)
      |> List.foldl fill D.empty
      |> D.insert (x, y) '.'
