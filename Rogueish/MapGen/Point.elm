module Rogueish.MapGen.Point exposing (..)

import List.Extra as LE


type alias X = Int


type alias Y = Int


type alias Point = (X, Y)


type alias Area = (Point, Point)


signum : number -> Int
signum x =
  if x < 0 then -1
  else if x == 0 then 0
  else 1


maxLevelDimExponent : Int
maxLevelDimExponent = 16


maxLevelDim : Int
maxLevelDim = 2 ^ maxLevelDimExponent - 1


chessDist : Point -> Point -> Int
chessDist (x0, y0) (x1, y1) = max (abs x1 - x0) (abs y1 - y0)


euclidDistSq : Point -> Point -> Int
euclidDistSq (x0, y0) (x1, y1) = ((x1 - x0) ^ 2) + ((y1 - y0) ^ 2)


adjacent : Point -> Point -> Bool
adjacent s t = chessDist s t == 1


inside : Area -> Point -> Bool
inside ((x0, y0), (x1, y1)) (x, y) = x1 >= x && x >= x0 && y1 >= y && y >= y0


{-| Bresenham's line algorithm
-}
bla : X -> Y -> Int -> Point -> Point -> Maybe (List Point)
bla lxsize lysize eps source target =
  if source == target then Nothing
  else
    let
      inBounds (x, y) =
        lxsize > x && x >= 0 && lysize > y && y >= 0 && (x, y) /= source
    in
      Just (LE.takeWhile inBounds (Maybe.withDefault [] <| List.tail (blaXY eps source target)))


blaXY : Int -> Point -> Point -> List Point
blaXY eps (x0, y0) (x1, y1) =
  let
    (dx, dy) =
      (x1 - x0, y1 - y0)

    xyStep b (x, y) =
      (x + signum dx, y + signum (dy * b))

    yxStep b (x, y) =
      (x + signum (dx * b), y + signum dy)

    (p, q, step) =
      if abs dx > abs dy then (abs dy, abs dx, xyStep) else (abs dx, abs dy, yxStep)

    bw =
      balancedWord p q (eps % (max 1 q)) (euclidDistSq (x0, y0) (x1, y1))

    walk w xy =
      case w of
        [] -> [xy]
        x :: xs -> xy :: walk xs (step x xy)
  in
    walk bw (x0, y0)


balancedWord : Int -> Int -> Int -> Int -> List Int
balancedWord p q eps length =
  if length == 1 then
    if eps + p < q then [0] else [1]
  else if eps + p < q then
    0 :: balancedWord p q (eps + p) (length - 1)
  else
    1 :: balancedWord p q (eps + p - q) (length - 1)


fromTo : Point -> Point -> List Point
fromTo (x0, y0) (x1, y1) =
  let
    fromTo' : Int -> Int -> List Int
    fromTo' z0 z1 =
      if z0 <= z1 then [z0..z1]
      else [z1..z0]
  in
    if x0 == x1 then List.map ((,) x0) (fromTo' y0 y1)
    else if y0 == y1 then List.map (\x -> (,) x y0) (fromTo' x0 x1)
    else Debug.crash "diagonal fromTo"


originPoint : Point
originPoint = (0, 0)
