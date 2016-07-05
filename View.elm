module View exposing (view)

import Model exposing (Size, Model, Enemy)
import Html exposing (Html)
import Svg exposing (Svg, svg, rect, text, text')
import Svg.Attributes exposing (height, width, x, y, fill)
import List exposing (map)


view : Size -> Model -> Html msg
view size model =
  let
    (w, h) =
      (toFloat size.width, toFloat size.height)

    (w', h') =
      (toString size.width, toString size.height)

    x' =
      toString (model.hero.x)

    y' =
      toString (h - model.hero.y - 50 - 30)
  in
    svg
      [ height h'
      , width w'
      ]
      ([ rect
        [ height h'
        , width w'
        , x "0"
        , y "0"
        , fill "#AEEEEE"
        ] []
      , rect
        [ height "50"
        , width w'
        , x "0"
        , y (toString (h - 50))
        , fill "#4AA72B"
        ] []
      , text'
        [ y "30"
        , x (toString (w - 50))
        , fill "#FFFFFF"
        ]
        [ text (toString model.score)
        ]
      , rect
        [ height "30"
        , width "20"
        , x x'
        , y y'
        , fill "#00FF00"
        ] []
      ] ++ (map (drawEnemy (w, h)) model.enemies))


drawEnemy : (Float, Float) -> Enemy -> Svg msg
drawEnemy (w, h) enemy =
  let
    x' =
      toString enemy.x

    y' =
      toString (h - 50 - 25)
  in
    rect
      [ height "25"
      , width "20"
      , x x'
      , y y'
      , fill "#FF0000"
      ] []
