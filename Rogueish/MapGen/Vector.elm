module Rogueish.MapGen.Vector exposing (..)

import Rogueish.MapGen.Point as P exposing (Point, X, Y)
import Dict as D
import Set as S exposing (Set)
import List.Extra as LE


type alias Vector = (X, Y)


maxVectorDim : Int
maxVectorDim = 2 ^ (P.maxLevelDimExponent - 1) - 1


isUnit : Vector -> Bool
isUnit v = chessDistVector v == 1


isDiagonal : Vector -> Bool
isDiagonal (x, y) = x * y /= 0


neg : Vector -> Vector
neg (vx, vy) = (-vx, -vy)


euclidDistSqVector : Vector -> Vector -> Int
euclidDistSqVector (x0, y0) (x1, y1) = ((x1 - x0) ^ 2) + ((y1 - y0) ^ 2)


chessDistVector : Vector -> Int
chessDistVector (x, y) = max (abs x) (abs y)


moves : List Vector
moves = [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]


moveTexts : List String
moveTexts = ["NW", "N", "NE", "E", "SE", "S", "SW", "W"]


compassText : Vector -> String
compassText v =
  LE.zip moves moveTexts
    |> D.fromList
    |> D.get v
    |> Maybe.withDefault ""


movesCardinal : List Vector
movesCardinal = [(0, -1), (1, 0), (0, 1), (-1, 0)]


movesDiagonal : List Vector
movesDiagonal = [(-1, -1), (1, -1), (1, 1), (-1, 1)]


vicinity : X -> Y -> Point -> List Point
vicinity lxsize lysize p =
  if P.inside ((1, 1), (lxsize - 2, lysize - 2)) p then
    vicinityUnsafe p
  else
    List.map (shift p) moves
      |> List.filter (P.inside ((0, 0), (lxsize - 1, lysize - 1)))


vicinityUnsafe : Point -> List Point
vicinityUnsafe p =
  List.map (shift p) moves


vicinityCardinal : X -> Y -> Point -> List Point
vicinityCardinal lxsize lysize p =
  List.map (shift p) movesCardinal
    |> List.filter (P.inside ((0, 0), (lxsize - 1, lysize - 1)))


squareUnsafeSet : Point -> Set Point
squareUnsafeSet (x, y) =
  List.concatMap (\y -> List.map (\x -> (x, y)) [x-1..x+1]) [y-1, y+1]
    |> S.fromList


shift : Point -> Vector -> Point
shift (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)


shiftBounded : X -> Y -> Point -> Vector -> Point
shiftBounded lxsize lysize pos (xv, yv) =
  if P.inside ((-xv, -yv), (lxsize - xv - 1, lysize - yv - 1)) pos then
    shift pos (xv, yv)
  else pos


trajectoryToPath : Point -> List Vector -> List Point
trajectoryToPath start vlist =
  case vlist of
    [] -> []
    v :: vs ->
      let
        next = shift start v
      in
        next :: trajectoryToPath next vs


trajectoryToPathBounded : X -> Y -> Point -> List Vector -> List Point
trajectoryToPathBounded lxsize lysize start vlist =
  case vlist of
    [] -> []
    v :: vs ->
      let
        next = shiftBounded lxsize lysize start v
      in
        next :: trajectoryToPathBounded lxsize lysize next vs


vectorToFrom : Point -> Point -> Vector
vectorToFrom (x0, y0) (x1, y1) = (x0 - x1, y0 - y1)


pathToTrajectory : List Point -> List Vector
pathToTrajectory points =
  case points of
    [] -> []
    p :: ps -> List.map2 vectorToFrom ps points


type alias RadianAngle = Float


rotate : RadianAngle -> Vector -> Vector
rotate angle (x', y') =
  let
    (x, y) =
      (toFloat x', toFloat y')

    (dx, dy) =
      (x * (cos -angle) - y * (sin -angle), x * (sin -angle) + y * (cos -angle))
  in
    normalize dx dy


normalize : Float -> Float -> Vector
normalize dx dy =
  let
    angle =
      atan (dy / dx) / (pi / 2)

    dxy =
      if angle <= -0.75 && angle >= -1.25 then (0, -1)
      else if angle <= -0.25 then (1, -1)
      else if angle <= 0.25 then (1, 0)
      else if angle <= 0.75 then (1, 1)
      else if angle <= 1.25 then (0, 1)
      else (0, 0)
  in
    if dx >= 0 then
      dxy
    else
      neg dxy


normalizeVector : Vector -> Vector
normalizeVector (vx, vy) =
  normalize (toFloat vx) (toFloat vy)


towards : Point -> Point -> Vector
towards pos0 pos1 =
  vectorToFrom pos1 pos0
    |> normalizeVector
