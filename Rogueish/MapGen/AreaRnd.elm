module Rogueish.MapGen.AreaRnd exposing (..)

import Rogueish.MapGen.Point as P exposing (Point, Area, X, Y)
import Rogueish.MapGen.Area as A
import Rogueish.MapGen.Vector as V exposing (Vector)
import Random as R exposing (Generator)
import Random.Extra as RE
import Random.Set as RS
import Set as S exposing (Set)


xyInArea : Area -> Generator Point
xyInArea ((x0, y0), (x1, y1)) =
  R.pair (R.int x0 x1) (R.int y0 y1)


makeRoom : (X, Y) -> (X, Y) -> Area -> Generator Area
makeRoom (xmin, ymin) (xmax, ymax) ((x0, y0), (x1, y1)) =
  let
    xmin' =
      min xmin (x1 - x0 + 1)

    ymin' =
      min ymin (y1 - y0 + 1)

    areaW =
      (xmin', ymin', min xmax (x1 - x0 + 1), min ymax (y1 - y0 + 1))
        |> A.toArea
        |> Maybe.withDefault ((0, 0), (0, 0))
  in
    xyInArea areaW
      `R.andThen` (\(xW, yW) ->
        let
          area1 =
            (x0, y0, max x0 (x1 - xW + 1), max y0 (y1 - yW + 1))
              |> A.toArea
              |> Maybe.withDefault ((0, 0), (0, 0))
        in
          R.map2 A.toArea2 (xyInArea area1) (RE.constant (xW, yW))
      )


makeVoidRoom : Area -> Generator Area
makeVoidRoom area =
  let
    core =
      Maybe.withDefault area (A.shrink area)
  in
    R.map A.trivialArea (xyInArea core)


connectGrid : (X, Y) -> Generator (List (Point, Point))
connectGrid (nx, ny) =
  let
    unconnected =
      List.concatMap (\x -> List.map (\y -> (x, y)) [0..ny-1]) [0..nx-1]
        |> S.fromList
  in
    R.pair (R.int 0 (nx - 1)) (R.int 0 (ny - 1))
      `R.andThen` (\point ->
        let
          candidates = S.singleton point
        in
          connectGrid' (nx, ny) unconnected candidates []
      )


connectGrid' : (X, Y) -> Set Point -> Set Point -> List (Point, Point) -> Generator (List (Point, Point))
connectGrid' (nx, ny) unconnected candidates acc =
  if S.isEmpty candidates then
    R.map (List.map sortPoint) (RE.constant acc)
  else
    R.map (Maybe.withDefault (0, 0)) (RS.sample candidates)
      `R.andThen` (\c ->
        let
          ns = S.fromList <| V.vicinityCardinal nx ny c
          nu = S.remove c unconnected
          (nc, ds) = S.partition (flip S.member nu) ns
        in
          (if S.isEmpty ds then RE.constant identity
          else R.map (\d -> (::) (c, Maybe.withDefault (0, 0) d)) (RS.sample ds))
            `R.andThen` (\new -> connectGrid' (nx, ny) nu (S.remove c (S.union candidates nc)) (new acc))
      )


sortPoint : (Point, Point) -> (Point, Point)
sortPoint (a, b) = if a <= b then (a, b) else (b, a)


randomConnection : (X, Y) -> Generator (Point, Point)
randomConnection (nx, ny) =
  RE.choice True False
    `R.andThen` (\rb ->
      if rb || ny <= 1 then
        R.map (\rx ry -> ((rx, ry), (rx + 1, ry))) (R.int 0 (nx - 2)) (R.int 0 (ny - 1))
      else
        R.map (\rx ry -> ((rx, ry), (rx, ry + 1))) (R.int 0 (nx - 1)) (R.int 0 (ny - 2))
    )


type HV
  = Horiz
  | Vert


type alias Corridor = List Point


makeCorridor : HV -> Point -> Point -> Area -> Generator Corridor
makeCorridor hv (x0, y0) (x1, y1) b =
  xyInArea b
    `R.andThen` (\(rx, ry) ->
      RE.constant (case hv of
        Horiz -> [(x0, y0), (rx, y0), (rx, y1), (x1, y1)]
        Vert -> [(x0, y0), (x0, ry), (x1, ry), (x1, y1)]
      )
    )


{-connectPlaces : (Area, Area) -> (Area, Area) -> Generator Corridor
connectPlaces (sa, so) (ta, to) =
  let
    (_, _, sx1, sy1) =
      A.fromArea sa

    (_, _, sox1, soy1) =
      A.fromArea so

    (tx0, ty0, _, _) =
      A.fromArea ta

    (tox0, toy0, _, _) =
      A.fromArea to

    trim area =
      let
        (x0, y0, x1, y1) =
          fromArea area

        trim (v0, v1) =
          if v1 - v0 < 6 then (v0, v1)
          else if v1 - v0 < 8 then (v0 + 3, v1 - 3)
          else (v0 + 4, v1 - 4)

        (nx0, nx1) =
          trim (x0, x1)

        (ny0, ny1) =
          trim (y0, y1)
      in
        toArea (nx0, ny0, nx1, ny1)
          |> Maybe.withDefault ((0, 0), (0, 0))
  in
    xyInArea <| trim so
      `R.andThen` (\sx sy ->

      )-}
