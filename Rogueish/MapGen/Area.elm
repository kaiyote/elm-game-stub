module Rogueish.MapGen.Area exposing (..)

import Rogueish.MapGen.Point exposing (Area, Point, X, Y)


toArea : (X, Y, X, Y) -> Maybe Area
toArea (x0, y0, x1, y1) =
  if x0 <= x1 && y0 <= y1 then
    Just ((x0, y0), (x1, y1))
  else
    Nothing


toArea2 : Point -> (X, Y) -> Area
toArea2 (x, y) (sX, sY) = ((x, y), ((x + sX), (y + sY)))


fromArea : Area -> (X, Y, X, Y)
fromArea ((x0, y0), (x1, y1)) = (x0, y0, x1, y1)


trivialArea : Point -> Area
trivialArea point = (point, point)


grid : (X, Y) -> Area -> List (Point, Area)
grid (nx, ny) ((x0, y0), (x1, y1)) =
  let
    xd = x1 - x0 -- not + 1 because we need overlap
    yd = y1 - y0
  in
    List.concatMap (\x ->
      List.map (\y ->
        ((x, y), ((x0 + xd * x // nx, y0 + yd * y // ny), (x0 + xd * (x + 1) // nx, y0 + yd * (y + 1) // ny)))
      ) [0..ny-1]
    ) [0..nx-1]


shrink : Area -> Maybe Area
shrink ((x0, y0), (x1, y1)) =
  toArea (x0 + 1, y0 + 1, x1 - 1, y1 - 1)
